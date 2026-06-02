#!/usr/bin/env bash
# Canonical Python quality gate — harness-agnostic. Claude Code, codex, pi, the
# git pre-commit hook, and humans all call this so the rules live in one place.
#
#   scripts/py-quality.sh --fix <paths...>   format + lint-fix in place, then
#                                            typecheck (per-edit / interactive use)
#   scripts/py-quality.sh <paths...>         check-only: ruff format --check +
#                                            ruff check + basedpyright (commit gate)
#
# Paths may be files or directories; non-.py files are ignored. Exits non-zero if
# any check fails (basedpyright output goes to stderr).
set -uo pipefail

venv="$HOME/.venv/dev/bin"

# If the managed tools are absent (env not synced yet), skip rather than block.
if [[ ! -x "$venv/ruff" || ! -x "$venv/basedpyright" ]]; then
    echo "py-quality: ruff/basedpyright not found in ~/.venv/dev — run 'uv sync --all-packages'." >&2
    exit 0
fi

fix=0
if [[ "${1:-}" == "--fix" ]]; then
    fix=1
    shift
fi

targets=()
for p in "$@"; do
    if [[ -d "$p" ]]; then
        targets+=("$p")
    elif [[ -f "$p" && "$p" == *.py ]]; then
        targets+=("$p")
    fi
done
[[ ${#targets[@]} -eq 0 ]] && exit 0

rc=0
if [[ "$fix" -eq 1 ]]; then
    "$venv/ruff" format "${targets[@]}" >/dev/null 2>&1 || true
    "$venv/ruff" check --fix "${targets[@]}" >/dev/null 2>&1 || true
else
    "$venv/ruff" format --check "${targets[@]}" || rc=1
    "$venv/ruff" check "${targets[@]}" || rc=1
fi

if ! out=$("$venv/basedpyright" "${targets[@]}" 2>&1); then
    printf '%s\n' "$out" >&2
    rc=1
fi

exit "$rc"
