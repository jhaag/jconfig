#!/usr/bin/env bash
# Claude Code PostToolUse adapter: pull the edited file out of the tool payload
# (stdin JSON) and run the canonical quality gate with auto-fix. Exit 2 surfaces
# basedpyright errors back to the agent; the shared gate lives in scripts/.
set -uo pipefail

file=$(jq -r '.tool_input.file_path // empty' 2>/dev/null)
[ -n "$file" ] || exit 0

if "$CLAUDE_PROJECT_DIR/scripts/py-quality.sh" --fix "$file"; then
    exit 0
fi
exit 2
