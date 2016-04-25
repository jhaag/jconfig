# Change prefix
unbind C-b
set -g prefix C-a

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
