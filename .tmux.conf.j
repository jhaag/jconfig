# Change prefix
unbind C-b
set -g prefix C-Space
bind-key C-Space last-window

# Start numbering at 1
set -g base-index 1

# Allows for faster key repetition
set -s escape-time 0

# Set status bar
set -g status-bg black
set -g status-fg white
set -g status-left ""
set -g status-right "#[fg=green]#H"

# Split panes using | and -
unbind '"'
unbind %
bind | split-window -h
bind - split-window -v

# Alt control for switching panes
bind -n M-Left  select-pane -L
bind -n M-Right select-pane -R
bind -n M-Up    select-pane -U
bind -n M-Down  select-pane -D

# THINGS I NEED TO LEARN MORE ABOUT
setw -g monitor-activity on
set  -g visual-activity  on

set-window-option -g window-status-current-bg yellow

set -g default-terminal "screen-256color"

set -g status-justify centre
