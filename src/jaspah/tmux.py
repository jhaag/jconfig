"""Thin wrapper around the tmux CLI, shared by jaspah's orchestration commands.

The package shells out to ``tmux`` rather than holding a long-lived client, mirroring
``tmux/status.py``: every command is a one-shot subprocess, and context (session,
pane) is passed explicitly as a target argument.
"""

from __future__ import annotations

import subprocess


def tmux(*args: str, check: bool = True) -> str:
    """Run a tmux command and return its stdout with the trailing newline stripped."""
    proc = subprocess.run(
        ["tmux", *args],
        check=check,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return proc.stdout.rstrip("\n")


def display_message(fmt: str, target: str | None = None) -> str:
    """Expand a tmux format string in the context of ``target`` (or the active client)."""
    args = ["display-message"]
    if target is not None:
        args += ["-t", target]
    args += ["-p", fmt]
    return tmux(*args)


def has_session(name: str) -> bool:
    """Whether a session named ``name`` already exists."""
    proc = subprocess.run(
        ["tmux", "has-session", "-t", f"={name}"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return proc.returncode == 0
