#!/usr/bin/env bash

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     HOST_OS=linux
                ;;
    Darwin*)    HOST_OS=darwin
                ;;
    *)          HOST_OS="UNKNOWN:${unameOut}"
                echo "WARNING: Dotfile configuration not supported for ${HOST_OS}"
                ;;
esac

JCONFIG_ROOT="$HOME/jconfig"

source $JCONFIG_ROOT/scripts/utilities.sh

#=== General ===================================================================
#--- Solarized Dark Terminal Setup ---------------------------------------------
if [ "$HOST_OS" == "linux" ]; then
    # For now, we only have terminal color setup for linux
    cd

    # download the package for terminal solarized dark and set it up
    if [[ ! -e ~/gnome-terminal-colors-solarized/ ]]; then
        git clone https://github.com/sigurdga/gnome-terminal-colors-solarized.git
        cd gnome-terminal-colors-solarized
        ./set_dark.sh
    fi

    echo '[NOTE] If solarized dark is not the theme for the terminal, look into it <https://github.com/seebi/dircolors-solarized>'
    echo 'try using "eval `dircolors ~/.dir_color`"'
fi

#--- uv and Dev Venv Setup -----------------------------------------------------
DEV_VENV="$HOME/.venv/dev"

# Bootstrap uv if not installed
if ! command -v uv >/dev/null 2>&1; then
    echo -e "Installing uv package manager...\n"
    curl -LsSf https://astral.sh/uv/install.sh | sh
    # Source PATH for this session
    export PATH="$HOME/.local/bin:$PATH"
fi

# Create or update dev venv with latest Python
mkdir -p "$HOME/.venv"

if [[ -d "$DEV_VENV" ]]; then
    # Check if we should upgrade to a newer Python
    CURRENT_MINOR=$("$DEV_VENV/bin/python" -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')")
    # Get what uv considers the latest Python 3
    LATEST_PY=$(uv python find 3 2>/dev/null)
    if [[ -n "$LATEST_PY" ]]; then
        LATEST_MINOR=$("$LATEST_PY" -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')" 2>/dev/null)
        if [[ -n "$LATEST_MINOR" && "$CURRENT_MINOR" != "$LATEST_MINOR" ]]; then
            echo -e "Upgrading dev venv from Python $CURRENT_MINOR to $LATEST_MINOR...\n"
            rm -rf "$DEV_VENV"
        fi
    fi
fi

if [[ ! -d "$DEV_VENV" ]]; then
    echo -e "Creating dev virtual environment at $DEV_VENV with latest Python...\n"
    uv venv --python 3 "$DEV_VENV"
fi

# Install powerline-shell if not present in venv
if [[ ! -x "$DEV_VENV/bin/powerline-shell" ]]; then
    echo -e "Installing powerline-shell as a system tool...\n"
    uv tool install powerline-shell
fi

# Symlink custom opam_switch segment (dynamic path detection)
POWERLINE_PYTHON=$(find $(find $(uv tool dir) -name "*powerline*") -name "python" | awk '!arr[$1]++')
POWERLINE_SEGMENTS_DIR=$(find $(find $(uv tool dir) -name "*powerline*") -name "segments" | awk '!arr[$1]++')
OPAM_SEGMENT_LINK="$POWERLINE_SEGMENTS_DIR/opam_switch.py"
OUTSIDE_SEGMENT_LINK="$POWERLINE_SEGMENTS_DIR/outside.py"

if [[ ! -L "$OPAM_SEGMENT_LINK" ]] || [[ "$(readlink "$OPAM_SEGMENT_LINK")" != "$JCONFIG_ROOT/powerline_opam_switch.py" ]]; then
    [[ -e "$OPAM_SEGMENT_LINK" ]] && rm "$OPAM_SEGMENT_LINK"
    ln -s "$JCONFIG_ROOT/powerline_opam_switch.py" "$OPAM_SEGMENT_LINK"
    echo -e "Linked custom opam_switch segment.\n"
fi

if [[ ! -L "$OUTSIDE_SEGMENT_LINK" ]] || [[ "$(readlink "$OUTSIDE_SEGMENT_LINK")" != "$JCONFIG_ROOT/powerline_outside.py" ]]; then
    [[ -e "$OUTSIDE_SEGMENT_LINK" ]] && rm "$OUTSIDE_SEGMENT_LINK"
    ln -s "$JCONFIG_ROOT/powerline_outside.py" "$OUTSIDE_SEGMENT_LINK"
    echo -e "Linked custom outside segment.\n"
fi

echo -e "Dev venv configured with powerline-shell.\n"

#--- Legacy Cleanup ------------------------------------------------------------
# Remove old powerline-shell from user site-packages (installed via pip install --user)
if pip3 show powerline-shell 2>/dev/null | grep -q "Location:.*\.local"; then
    echo -e "Removing legacy user-level powerline-shell...\n"
    pip3 uninstall powerline-shell -y
fi

# Remove old symlink from system site-packages (if it exists)
LEGACY_SYMLINK="/usr/local/lib/python3.10/dist-packages/powerline_shell/segments/opam_switch.py"
if [[ -L "$LEGACY_SYMLINK" ]]; then
    echo -e "Removing legacy opam_switch symlink...\n"
    sudo rm "$LEGACY_SYMLINK"
fi

#=== Bash ======================================================================
# Add custom configs to .bashrc
read -r -d '' BASH_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my dotfiles ===========================================
source $JCONFIG_ROOT/.bashrc
EOF

load_custom_config "$BASH_CONF" ~/.bashrc "#"

#=== Emacs =====================================================================
echo "I now auto-generate my ~/.emacs file using org-babel; open ~/jconfig/.emacs.d/init.org and tangle the Bootstrap Process header."

#=== Git =======================================================================
# Add custom configs to .gitconfig
read -r -d '' GIT_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my dotfiles ===========================================
[include]
  path = $JCONFIG_ROOT/.gitconfig
EOF

load_custom_config "$GIT_CONF" ~/.gitconfig "#"

# Create user-specific gitignore if it doesn't exist
if [[ ! -f "$JCONFIG_ROOT/.gitconfig.user" ]]; then
    cat <<EOF > $JCONFIG_ROOT/.gitconfig.user
[user]
        name = Jasper Haag
        email = jasperhaag16@gmail.com
EOF
    touch "$JCONFIG_ROOT/.gitconfig.user"
fi

#=== Powerline Shell ===========================================================
# Remove old configuration if it exists
if [ -f ~/.powerline-shell.json ]; then
    rm ~/.powerline-shell.json
fi

# Remove old theme if it exists
if [ -f ~/.powerline-shell-theme.py ]; then
    rm ~/.powerline-shell-theme.py
fi

# Copy custom configs to ~/.powerline-shell.json
cp $JCONFIG_ROOT/.powerline-shell.json ~/.powerline-shell.json
# Copy custom theme to ~/.powerline-shell-theme.py
cp $JCONFIG_ROOT/.powerline-shell-theme.py ~/.powerline-shell-theme.py

#=== Tmux ======================================================================
# Add custom configs to .tmux.conf
read -r -d '' TMUX_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my configurations =====================================
source-file $JCONFIG_ROOT/.tmux.conf
EOF

load_custom_config "$TMUX_CONF" ~/.tmux.conf "#"

#=== Cron Jobs =================================================================
# Install crontabs from cron/ directory
CRON_DIR="$JCONFIG_ROOT/cron"

if [[ -d "$CRON_DIR" ]]; then
    CURRENT_CRONTAB=$(crontab -l 2>/dev/null || true)
    UPDATED_CRONTAB="$CURRENT_CRONTAB"
    CHANGED=false

    for cron_file in "$CRON_DIR"/*; do
        [[ ! -f "$cron_file" ]] && continue

        cron_name=$(basename "$cron_file")
        cron_content=$(cat "$cron_file")
        cron_marker="# jconfig:$cron_name"
        cron_entry="${cron_content} ${cron_marker}"

        # Check if marker exists in current crontab
        if echo "$UPDATED_CRONTAB" | grep -qF "$cron_marker"; then
            # Entry exists - check if it needs updating
            existing_line=$(echo "$UPDATED_CRONTAB" | grep -F "$cron_marker")
            if [[ "$existing_line" != "$cron_entry" ]]; then
                echo -e "Updating cron entry: $cron_name\n"
                UPDATED_CRONTAB=$(echo "$UPDATED_CRONTAB" | sed "s|.*${cron_marker}|${cron_entry}|")
                CHANGED=true
            else
                echo -e "Cron entry up to date: $cron_name\n"
            fi
        else
            # Entry doesn't exist - add it
            echo -e "Adding cron entry: $cron_name\n"
            UPDATED_CRONTAB="${UPDATED_CRONTAB}"$'\n'"${cron_entry}"
            CHANGED=true
        fi
    done

    # Install updated crontab if changed
    if [[ "$CHANGED" == true ]]; then
        echo "$UPDATED_CRONTAB" | crontab -
    fi
fi

#=== Claude Code ===============================================================
# Symlink Claude Code settings from repo to ~/.claude
# Using symlinks so edits via Claude's /memory, /settings, etc. go to the repo
mkdir -p ~/.claude

# Symlink settings.json
if [ -f $JCONFIG_ROOT/.claude-config/settings.json ]; then
    [ -f ~/.claude/settings.json ] && [ ! -L ~/.claude/settings.json ] && rm ~/.claude/settings.json
    [ ! -e ~/.claude/settings.json ] && ln -s "$JCONFIG_ROOT/.claude-config/settings.json" ~/.claude/settings.json
    echo "Claude Code settings.json symlinked."
fi

# Symlink CLAUDE.md
if [ -f $JCONFIG_ROOT/.claude-config/CLAUDE.md ]; then
    [ -f ~/.claude/CLAUDE.md ] && [ ! -L ~/.claude/CLAUDE.md ] && rm ~/.claude/CLAUDE.md
    [ ! -e ~/.claude/CLAUDE.md ] && ln -s "$JCONFIG_ROOT/.claude-config/CLAUDE.md" ~/.claude/CLAUDE.md
    echo "Claude Code CLAUDE.md symlinked."
fi

# Symlink commands directory
if [ -d $JCONFIG_ROOT/.claude-config/commands ]; then
    [ -d ~/.claude/commands ] && [ ! -L ~/.claude/commands ] && rm -rf ~/.claude/commands
    [ ! -e ~/.claude/commands ] && ln -s "$JCONFIG_ROOT/.claude-config/commands" ~/.claude/commands
    echo "Claude Code custom commands symlinked."
fi

# Idea! Put in todo variables througout configs, and have this file go through and sed everything
# Another Idea! Stamp config files with the time that they were configured, and compare on the fly
# to determine if it is necessary to add the custom stuff in, or if we want to do something else
# (like remove the old custom stuff and add it again so it doesn't stack up)
