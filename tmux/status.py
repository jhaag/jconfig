#!/usr/bin/env python3
"""Render and install the jaspah tmux status area.

The tmux config invokes this script once per refresh. The script computes the
entire multiline status area, then sets tmux's `status` and `status-format[]`
options for the current session in one pass.
"""

from __future__ import annotations

import argparse
import datetime as dt
import re
import subprocess
import unicodedata
from dataclasses import dataclass
from pathlib import Path

MANAGED_VENV = "/home/jhaag/.venv/dev"
UV = "/home/jhaag/.local/bin/uv"
PYTHON = str(Path(__file__).resolve())
REFRESH_CMD = (
    f"VIRTUAL_ENV={MANAGED_VENV} PATH={MANAGED_VENV}/bin:$PATH "
    f"{UV} run --active python {PYTHON} "
    "--quiet --session '#{session_id}' --window '#{window_id}'"
)
DRIVER = f"#({REFRESH_CMD})"

INFO_MIN_WIDTH = 20
# The info pane grows to fit its content (e.g. a large uptime) but never takes
# more than this share of the bar, so the window list keeps usable room.
INFO_MAX_FRACTION = 0.5
INFO_PAD_LEFT = 1
INFO_PAD_RIGHT = 1
INFO_COLS = 2  # info items pack into this many aligned columns to minimise rows
WIN_MAX = 25
DENY = {"bash", "claude"}

# tmux hard-caps the `status` option at 5: setting it higher errors with
# "unknown value". This is tmux's physical ceiling, not a layout choice — the
# renderer uses the *fewest* lines it can and only grows toward this bound.
TMUX_MAX_LINES = 5
# Wrap the window list to another row once a row would exceed this fraction of
# the available width; keeps rows from filling edge-to-edge before wrapping.
WIN_ROW_FILL = 0.9

# tmux style/glyph palette. Keep this duplicated here deliberately: the status
# renderer should not depend on tmux-side user options expanding correctly.
LIGHT2 = "colour250"
BRIGHT_CYAN = "colour51"
BRIGHT_GREEN = "colour142"
BRIGHT_RED = "colour167"
DARK0 = "colour235"

THREAD = "🧵"
BULLSEYE = "◎"
UP_TRIANGLE = "▲"
# Two dot sizes set the separator hierarchy: a bullet divides the info pane
# from the window list, a small floating dot divides info columns.
INFO_WINDOW_SEP = "•"
INFO_ITEM_SEP = "·"
# Single-letter weekday codes, the MTWRF(SU) convention (R=Thu, S=Sat, U=Sun),
# indexed by datetime.weekday() (Mon=0 .. Sun=6).
DAY_CODES = "MTWRFSU"
OUT_HORIZONTAL = "═"
OUT_VERTICAL = "║"
OUT_DTEE = "╤"
OUT_BLCORNER = "╚"
OUT_BRCORNER = "╝"

STYLE_RE = re.compile(r"#\[[^]]*\]")


@dataclass(frozen=True)
class Context:
    session_id: str
    session_name: str
    window_id: str
    window_width: int
    host: str
    host_short: str


@dataclass(frozen=True)
class RowLayout:
    """Column budget for one rendered status row."""

    total_width: int
    info_width: int
    window_width: int

    @classmethod
    def from_parts(cls, total_width: int, info: list[str]) -> RowLayout:
        content_width = max((display_width(row) for row in info), default=0)
        ceiling = max(INFO_MIN_WIDTH, int(total_width * INFO_MAX_FRACTION))
        info_width = min(ceiling, max(INFO_MIN_WIDTH, content_width))
        # ║ + left padding + info + right padding + • + spacer + window-list + ║
        fixed_width = (
            display_width(OUT_VERTICAL)
            + INFO_PAD_LEFT
            + info_width
            + INFO_PAD_RIGHT
            + display_width(INFO_WINDOW_SEP)
            + 1
            + display_width(OUT_VERTICAL)
        )
        return cls(
            total_width=total_width,
            info_width=info_width,
            window_width=max(0, total_width - fixed_width),
        )


@dataclass(frozen=True)
class Window:
    index: str
    active: bool
    flags: str
    name: str
    automatic_rename: bool
    pane_command: str
    pane_title: str

    @property
    def label(self) -> str:
        title = self.pane_title.strip()
        command = self.pane_command.strip()
        if title and title not in {HOST, HOST_SHORT}:
            candidate = title
        elif self.name.strip():
            candidate = self.name.strip()
        else:
            candidate = command
        if candidate in DENY and command:
            return command
        return candidate or command or self.name or "?"


HOST = ""
HOST_SHORT = ""


def tmux(*args: str, check: bool = True) -> str:
    proc = subprocess.run(
        ["tmux", *args],
        check=check,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return proc.stdout.rstrip("\n")


def tmux_escape(text: str) -> str:
    """Escape literal text for inclusion in a tmux format string."""
    return text.replace("#", "##")


def display_width(text: str) -> int:
    text = STYLE_RE.sub("", text)
    # In tmux format strings, ## renders as one literal #.
    text = text.replace("##", "#")
    width = 0
    for ch in text:
        if unicodedata.combining(ch):
            continue
        if unicodedata.category(ch) in {"Mn", "Me", "Cf"}:
            continue
        width += 2 if unicodedata.east_asian_width(ch) in {"F", "W"} else 1
    return width


def truncate(text: str, max_width: int) -> str:
    if display_width(text) <= max_width:
        return text
    out = ""
    width = 0
    for ch in text:
        ch_width = 2 if unicodedata.east_asian_width(ch) in {"F", "W"} else 1
        if width + ch_width >= max_width:
            return out + "…"
        out += ch
        width += ch_width
    return out


def pad_right(text: str, width: int) -> str:
    text = truncate(text, width)
    return text + " " * max(0, width - display_width(text))


def center(text: str, width: int) -> str:
    text = truncate(text, width)
    extra = max(0, width - display_width(text))
    left = extra // 2
    return " " * left + text + " " * (extra - left)


def split_fields(line: str, count: int) -> list[str]:
    fields = line.split("\t")
    if len(fields) < count:
        fields.extend([""] * (count - len(fields)))
    return fields[:count]


def get_context(session: str | None, window: str | None) -> Context:
    target = window or session or ""
    fmt = "#{session_id}\t#{session_name}\t#{window_id}\t#{window_width}\t#{host}\t#{host_short}"
    args = (
        ["display-message", "-p", fmt]
        if not target
        else ["display-message", "-t", target, "-p", fmt]
    )
    fields = split_fields(tmux(*args), 6)
    return Context(
        session_id=fields[0],
        session_name=fields[1],
        window_id=fields[2],
        window_width=int(fields[3] or "0"),
        host=fields[4],
        host_short=fields[5],
    )


def get_windows(session_id: str) -> list[Window]:
    fmt = "#{window_index}\t#{window_active}\t#{window_flags}\t#{window_name}\t#{automatic-rename}\t#{pane_current_command}\t#{pane_title}"
    rows = tmux("list-windows", "-t", session_id, "-F", fmt).splitlines()
    windows: list[Window] = []
    for row in rows:
        index, active, flags, name, automatic, command, title = split_fields(row, 7)
        windows.append(
            Window(
                index=index,
                active=active == "1",
                flags=flags,
                name=name,
                automatic_rename=automatic == "1",
                pane_command=command,
                pane_title=title,
            )
        )
    return windows


def normalize_window_flags(flags: str) -> str:
    """Collapse duplicate tmux window flag glyphs while preserving order."""
    seen: set[str] = set()
    normalized: list[str] = []
    for flag in flags:
        if flag in seen:
            continue
        seen.add(flag)
        normalized.append(flag)
    return "".join(normalized)


def window_segment(window: Window) -> str:
    sep_style = f"#[dim,fg={LIGHT2}]" if not window.active else ""
    text_style = f"#[fg={LIGHT2}]" if not window.active else ""
    default = "#[default]"
    label = tmux_escape(truncate(window.label, WIN_MAX))
    flags = tmux_escape(normalize_window_flags(window.flags))
    if window.active:
        return (
            f" {BULLSEYE} #[fg={LIGHT2}]{window.index}:"
            f"{default}{label}#[bold,fg={BRIGHT_GREEN}]{flags}{default}"
        )
    return (
        f"{sep_style} {BULLSEYE} {default}{text_style}{window.index}:"
        f"{label}#[bold,fg={BRIGHT_GREEN}]{flags}{default}"
    )


def choose_groups(total: int, available: int, max_rows: int) -> int:
    """Pick the fewest window rows that keep each row within WIN_ROW_FILL of the
    available width, capped at max_rows."""
    if total <= 0 or available <= 0:
        return 1
    capacity = max(1, int(available * WIN_ROW_FILL))
    needed = -(-total // capacity)  # ceil division
    return max(1, min(max_rows, needed))


def pack_segments(segments: list[str], groups: int) -> list[str]:
    """Balanced greedy pack of window segments into at most `groups` rows.

    Every segment lands in exactly one row and no row is discarded. The break is
    gated on the group budget (`rows_still_to_open`), so the loop can never open
    more rows than `groups`; the trailing flush emits whatever remains.
    """
    total = sum(display_width(s) for s in segments)
    target = max(1, -(-total // groups))  # ceil(total / groups)
    rows: list[str] = []
    current: list[str] = []
    current_width = 0
    for idx, segment in enumerate(segments):
        seg_width = display_width(segment)
        remaining_segments = len(segments) - idx
        rows_still_to_open = groups - len(rows) - 1
        if (
            current
            and current_width + seg_width > target
            and rows_still_to_open > 0
            and remaining_segments > rows_still_to_open
        ):
            rows.append("".join(current))
            current = []
            current_width = 0
        current.append(segment)
        current_width += seg_width
    rows.append("".join(current))
    return rows


def window_rows_needed(segments: list[str], available_width: int, max_rows: int) -> int:
    """Fewest rows that hold every window segment within the width, capped at
    max_rows. Zero when there are no windows."""
    if not segments:
        return 0
    total = sum(display_width(s) for s in segments)
    return min(choose_groups(total, available_width, max_rows), len(segments), max_rows)


def split_window_rows(
    segments: list[str], available_width: int, n_rows: int
) -> list[str]:
    """Lay window segments into exactly `n_rows` cells.

    Segments pack into the fewest rows that fit the width, then centre
    vertically within the cells (top-biased, so a single row sits in the
    middle). The result always has length `n_rows`; unused cells are empty.
    """
    if n_rows <= 0:
        return []
    if not segments:
        return [""] * n_rows

    total = sum(display_width(s) for s in segments)
    groups = min(choose_groups(total, available_width, n_rows), len(segments), n_rows)
    content = pack_segments(segments, groups)
    # Grow the row count until every row fits, while spare cells remain.
    while (
        available_width > 0
        and groups < min(n_rows, len(segments))
        and any(display_width(row) > available_width for row in content)
    ):
        groups += 1
        content = pack_segments(segments, groups)

    pad = n_rows - len(content)
    top = (pad + 1) // 2
    return [""] * top + content + [""] * (pad - top)


def uptime_days() -> int:
    try:
        with open("/proc/uptime", encoding="utf-8") as file:
            return int(float(file.read().split()[0]) // 86400)
    except OSError:
        return 0


def info_items(ctx: Context) -> list[str]:
    """The atomic info pieces, ordered left-to-right then top-to-bottom."""
    days = uptime_days()
    uptime_colour = BRIGHT_RED if days >= 7 else BRIGHT_CYAN
    now = dt.datetime.now()
    return [
        f"{THREAD} {tmux_escape(ctx.session_name)}",
        f"{UP_TRIANGLE} #[fg={uptime_colour}]{days}☀#[default]",
        f"{DAY_CODES[now.weekday()]} {now:%d %b '%y}",
        now.strftime("%H:%M:%S"),
    ]


def info_rows(ctx: Context) -> list[str]:
    """Pack info items into an INFO_COLS-wide grid, minimising rows.

    Each column is padded to its widest cell so the floating item dividers stack
    vertically; the trailing cell of a row is left ragged (status_row pads the
    whole row). A short final row simply omits the missing columns.
    """
    items = info_items(ctx)
    grid = [items[i : i + INFO_COLS] for i in range(0, len(items), INFO_COLS)]
    col_widths = [
        max((display_width(row[col]) for row in grid if col < len(row)), default=0)
        for col in range(INFO_COLS)
    ]
    divider = f"#[dim,fg={LIGHT2}] {INFO_ITEM_SEP} #[default]"
    rows: list[str] = []
    for cells in grid:
        parts = [
            pad_right(cell, col_widths[col]) if col < len(cells) - 1 else cell
            for col, cell in enumerate(cells)
        ]
        rows.append(divider.join(parts))
    return rows


def pane_separators(window_id: str, window_width: int) -> set[int]:
    fmt = "#{pane_left}\t#{pane_right}\t#{pane_top}"
    seps: set[int] = set()
    for row in tmux("list-panes", "-t", window_id, "-F", fmt).splitlines():
        left_s, right_s, top_s = split_fields(row, 3)
        left, right, top = int(left_s), int(right_s), int(top_s)
        # Only separators touching the top of the tmux window can connect down
        # from the status separator line.
        if top != 0:
            continue
        if left > 0:
            seps.add(left - 1)
        if right < window_width - 1:
            seps.add(right + 1)
    return seps


def separator_row(ctx: Context) -> str:
    seps = pane_separators(ctx.window_id, ctx.window_width)
    chars: list[str] = []
    last = ctx.window_width - 1
    for col in range(ctx.window_width):
        if col == 0:
            chars.append(OUT_BLCORNER)
        elif col == last:
            chars.append(OUT_BRCORNER)
        elif col in seps:
            chars.append(OUT_DTEE)
        else:
            chars.append(OUT_HORIZONTAL)
    return "".join(chars)


def status_row(info: str, windows: str, layout: RowLayout) -> str:
    """Render one exact-width status row with aligned outer borders."""
    row = (
        f"{OUT_VERTICAL}"
        f"{' ' * INFO_PAD_LEFT}"
        f"{pad_right(info, layout.info_width)}"
        f"{' ' * INFO_PAD_RIGHT}"
        f"#[fg={LIGHT2}]{INFO_WINDOW_SEP}#[default] "
        f"{center(windows, layout.window_width)}"
        f"{OUT_VERTICAL}"
    )
    # Keep the right border pinned even if future glyph/width changes are made.
    width = display_width(row)
    if width < layout.total_width:
        row = row[:-1] + " " * (layout.total_width - width) + OUT_VERTICAL
    elif width > layout.total_width:
        row = truncate(row[:-1], layout.total_width - 1) + OUT_VERTICAL
    return row


def build_rows(ctx: Context) -> list[str]:
    """Lay out the status area: an info column beside a window list, closed by a
    separator. The line count is the minimum that holds both — it grows only
    when the window list needs more rows than the info column, up to tmux's
    line limit (the last line is always the separator)."""
    info = info_rows(ctx)
    layout = RowLayout.from_parts(ctx.window_width, info)

    windows = get_windows(ctx.session_id)
    segments = [window_segment(window) for window in windows]

    # Reserve the final line for the separator; everything above is content.
    content_budget = TMUX_MAX_LINES - 1
    needed = window_rows_needed(segments, layout.window_width, content_budget)
    content_rows = min(content_budget, max(len(info), needed))

    info_cells = (info + [""] * content_rows)[:content_rows]
    win_cells = split_window_rows(segments, layout.window_width, content_rows)

    rows = [
        status_row(left, right, layout)
        for left, right in zip(info_cells, win_cells, strict=True)
    ]
    rows.append(separator_row(ctx))
    return rows


def apply_status(ctx: Context) -> None:
    rows = build_rows(ctx)
    # tmux status lines are 0-indexed: `status N` shows status-format[0..N-1].
    # The count and every row are set in one atomic command, so each index is
    # populated before tmux can render it; no higher index needs clearing
    # because a smaller count simply stops rendering the trailing rows.
    commands = ["set-option", "-t", ctx.session_id, "-q", "status", str(len(rows))]
    for idx, row in enumerate(rows):
        if idx == 0:
            row = row + DRIVER
        commands.extend(
            [
                ";",
                "set-option",
                "-t",
                ctx.session_id,
                "-q",
                f"status-format[{idx}]",
                row,
            ]
        )
    tmux(*commands)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--session")
    parser.add_argument("--window")
    parser.add_argument("--quiet", action="store_true")
    args = parser.parse_args()

    ctx = get_context(args.session, args.window)
    global HOST, HOST_SHORT
    HOST = ctx.host
    HOST_SHORT = ctx.host_short
    apply_status(ctx)
    if not args.quiet:
        print(f"updated tmux status for {ctx.session_name} ({ctx.window_id})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
