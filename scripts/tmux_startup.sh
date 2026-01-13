#!/usr/bin/env bash

SESSION="default"

# Check if session exists
tmux has-session -t $SESSION 2>/dev/null

if [ $? != 0 ]; then
    # Window 1: org (landing page for orgmode)
    tmux new-session -d -s $SESSION -n "org"

    # Window 2: dev (split: both panes in ~/dev)
    tmux new-window -t $SESSION:2 -n "dev"
    tmux send-keys -t $SESSION:2.1 "cd ~/dev; clear" C-m
    tmux split-window -h -t $SESSION:2
    tmux send-keys -t $SESSION:2.2 "cd ~/dev; clear" C-m
    tmux select-pane -t $SESSION:2.1

    # Select first window (org)
    tmux select-window -t $SESSION:1
fi

# Attach to session
tmux attach-session -t $SESSION
