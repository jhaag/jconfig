#!/usr/bin/env bash

jconfig_location="$HOME/jconfig"

source $jconfig_location/scripts/utilities.sh

#=== General ===============================================================
#--- Solarized Dark Terminal Setup -----------------------------------------
cd

# download the package for terminal solarized dark and set it up
if [[ ! -e ~/gnome-terminal-colors-solarized/ ]]; then
    git clone gh:sigurdga/gnome-terminal-colors-solarized.git
    cd gnome-terminal-colors-solarized
    ./set_dark.sh
fi

echo '[NOTE] If solarized dark is not the theme for the terminal, look into it'

#--- Powerline Font Terminal Setup -----------------------------------------
cd

# download the package for the fonts I want and set them up
if ! [[ -e ~/fonts/ ]]; then
    git clone https://github.com/powerline/fonts.git
    cd fonts
    ./install.sh
fi

echo '[NOTE] Change your user preferences to use Meslo LG S at 11pt font'


echo -e '\n\n'
#=== Bash ==================================================================
# Add custom configs to .bashrc
read -r -d '' BASH_CONF <<EOF
#=== Custom global configurations ======

#=== Hook for pulling in my dotfiles ===
source $jconfig_location/.bashrc
EOF

load_custom_config "$BASH_CONF" ~/.bashrc "#"

#=== Emacs =================================================================
# Add custom configs to .emacs
read -r -d '' EMACS_CONF <<EOF
;;=== Custom global configurations ==========

;;=== Hook for pulling in my dotfiles =======
(load "$jconfig_location/.emacs.d/init.el")

;;=== Non-standard custom-file setup ========
(setq custom-file "$jconfig_location/.emacs.d/.emacs-custom.el")
(load custom-file)
EOF

load_custom_config "$EMACS_CONF" ~/.emacs ";;"

#=== Git ====================================================================
# Add custom configs to .gitconfig
read -r -d '' GIT_CONF <<EOF
#=== Custom global configurations ======

#=== Hook for pulling in my dotfiles ===
[include]
  path = ~/jconfig/.gitconfig
EOF

load_custom_config "$GIT_CONF" ~/.gitconfig "#"

#=== Tmux ======================================================================
# Add custom configs to .tmux.conf
read -r -d '' TMUX_CONF <<EOF
#=== Custom global configurations ============

#=== Hook for pulling in my configurations ===
source-file ~/jconfig/.tmux.conf
EOF

load_custom_config "$TMUX_CONF" ~/.tmux.conf "#"

# Idea! Put in todo variables througout configs, and have this file go through and sed everything
# Another Idea! Stamp config files with the time that they were configured, and compare on the fly to determine if it is necessary to add the custom stuff in, or if we want to do something else (like remove the old custom stuff and add it again so it doesn't stack up)
