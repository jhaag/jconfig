#!/usr/bin/env bash

SESSION="default"

# Check if session exists
tmux has-session -t $SESSION 2>/dev/null

if [ $? != 0 ]; then
    # Determine which body file to source
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    BODY_OVERRIDE="$SCRIPT_DIR/tmux_startup_body.sh"
    BODY_DEFAULT="$SCRIPT_DIR/tmux_startup_body.sh.default"

    # Use override if it exists, otherwise use default
    if [ -f "$BODY_OVERRIDE" ]; then
        source "$BODY_OVERRIDE"
    elif [ -f "$BODY_DEFAULT" ]; then
        source "$BODY_DEFAULT"
    else
        echo "Error: Neither tmux_startup_body.sh nor tmux_startup_body.sh.default found in $SCRIPT_DIR" >&2
        exit 1
    fi
fi

# Attach to session
tmux attach-session -t $SESSION
