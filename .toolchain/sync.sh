#!/usr/bin/env bash
# Non-interactive sync: applies repo state to the live system (dev venv, powerline,
# Herdr config, bash/git/tmux include blocks, gitconfig.user, crontabs).
# Reads cached identity written by bootstrap.sh.

: "${JASPAH_ROOT:?must be set by configure.sh}"

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     HOST_OS=linux ;;
    Darwin*)    HOST_OS=darwin ;;
    *)          HOST_OS="UNKNOWN:${unameOut}" ;;
esac

source "$JASPAH_ROOT/scripts/utilities.sh"

# Load cached identity (written by bootstrap.sh)
JASPAH_USER_ENV="$HOME/.config/jaspah/user.env"
[[ -f "$JASPAH_USER_ENV" ]] && source "$JASPAH_USER_ENV"

#=== Dev Venv ==================================================================
DEV_VENV="$HOME/.venv/dev"
mkdir -p "$HOME/.venv"

echo -e "Syncing dev venv at $DEV_VENV...\n"
UV_PROJECT_ENVIRONMENT="$DEV_VENV" uv sync --project "$JASPAH_ROOT"

#=== Powerline Shell ===========================================================
# Install as a uv tool (not into the project venv) so its deps stay isolated.
if ! uv tool list 2>/dev/null | grep -q '^powerline-shell '; then
    echo -e "Installing powerline-shell as a uv tool...\n"
    uv tool install powerline-shell
fi

# Symlink custom segments into the tool's segments dir (dynamic path detection)
POWERLINE_PYTHON=$(find $(find $(uv tool dir) -name "*powerline*") -name "python" | sort -V | tail -1)
POWERLINE_SEGMENTS_DIR=$(find $(find $(uv tool dir) -name "*powerline*") -name "segments" | sort -V | tail -1)
OPAM_SEGMENT_LINK="$POWERLINE_SEGMENTS_DIR/opam_switch.py"
OUTSIDE_SEGMENT_LINK="$POWERLINE_SEGMENTS_DIR/outside.py"

if [[ ! -L "$OPAM_SEGMENT_LINK" ]] || [[ "$(readlink "$OPAM_SEGMENT_LINK")" != "$JASPAH_ROOT/powerline/powerline_opam_switch.py" ]]; then
    [[ -e "$OPAM_SEGMENT_LINK" || -L "$OPAM_SEGMENT_LINK" ]] && rm "$OPAM_SEGMENT_LINK"
    ln -s "$JASPAH_ROOT/powerline/powerline_opam_switch.py" "$OPAM_SEGMENT_LINK"
    echo -e "Linked custom opam_switch segment.\n"
fi

if [[ ! -L "$OUTSIDE_SEGMENT_LINK" ]] || [[ "$(readlink "$OUTSIDE_SEGMENT_LINK")" != "$JASPAH_ROOT/powerline/powerline_outside.py" ]]; then
    [[ -e "$OUTSIDE_SEGMENT_LINK" || -L "$OUTSIDE_SEGMENT_LINK" ]] && rm "$OUTSIDE_SEGMENT_LINK"
    ln -s "$JASPAH_ROOT/powerline/powerline_outside.py" "$OUTSIDE_SEGMENT_LINK"
    echo -e "Linked custom outside segment.\n"
fi

# Symlink config and theme, replacing stale/broken links from previous repo paths.
POWERLINE_CONFIG_SRC="$JASPAH_ROOT/powerline/.powerline-shell.json"
POWERLINE_CONFIG_DEST="$HOME/.powerline-shell.json"
POWERLINE_THEME_SRC="$JASPAH_ROOT/powerline/.powerline-shell-theme.py"
POWERLINE_THEME_DEST="$HOME/.powerline-shell-theme.py"

if [[ ! -L "$POWERLINE_CONFIG_DEST" ]] || [[ "$(readlink "$POWERLINE_CONFIG_DEST")" != "$POWERLINE_CONFIG_SRC" ]]; then
    [[ -e "$POWERLINE_CONFIG_DEST" || -L "$POWERLINE_CONFIG_DEST" ]] && rm "$POWERLINE_CONFIG_DEST"
    ln -s "$POWERLINE_CONFIG_SRC" "$POWERLINE_CONFIG_DEST"
    echo -e "Linked powerline-shell config.\n"
fi

if [[ ! -L "$POWERLINE_THEME_DEST" ]] || [[ "$(readlink "$POWERLINE_THEME_DEST")" != "$POWERLINE_THEME_SRC" ]]; then
    [[ -e "$POWERLINE_THEME_DEST" || -L "$POWERLINE_THEME_DEST" ]] && rm "$POWERLINE_THEME_DEST"
    ln -s "$POWERLINE_THEME_SRC" "$POWERLINE_THEME_DEST"
    echo -e "Linked powerline-shell theme.\n"
fi

#=== Herdr =====================================================================
HERDR_CONFIG_DIR="$HOME/.config/herdr"
HERDR_CONFIG_SRC="$JASPAH_ROOT/herdr/config.toml"
HERDR_CONFIG_DEST="$HERDR_CONFIG_DIR/config.toml"

mkdir -p "$HERDR_CONFIG_DIR"

if [[ ! -L "$HERDR_CONFIG_DEST" ]] || [[ "$(readlink "$HERDR_CONFIG_DEST")" != "$HERDR_CONFIG_SRC" ]]; then
    [[ -e "$HERDR_CONFIG_DEST" || -L "$HERDR_CONFIG_DEST" ]] && rm "$HERDR_CONFIG_DEST"
    ln -s "$HERDR_CONFIG_SRC" "$HERDR_CONFIG_DEST"
    echo -e "Linked Herdr config.\n"
fi

#=== Bash ======================================================================
read -r -d '' BASH_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my dotfiles ===========================================
source $JASPAH_ROOT/bash/.bashrc
EOF

load_custom_config "$BASH_CONF" ~/.bashrc "#"

#=== Git =======================================================================
read -r -d '' GIT_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my dotfiles ===========================================
[include]
  path = $JASPAH_ROOT/git/.gitconfig
EOF

load_custom_config "$GIT_CONF" ~/.gitconfig "#"

# Always regenerate .gitconfig.user from cached identity
cat <<EOF > "$JASPAH_ROOT/git/.gitconfig.user"
[user]
        name = $JASPAH_NAME
        email = $JASPAH_EMAIL
EOF

#=== Tmux ======================================================================
read -r -d '' TMUX_CONF <<EOF
#=== Custom global configurations ==============================================

#=== Hook for pulling in my configurations =====================================
source-file $JASPAH_ROOT/tmux/.tmux.conf
EOF

load_custom_config "$TMUX_CONF" ~/.tmux.conf "#"

#=== Cron Jobs =================================================================
CRON_DIR="$JASPAH_ROOT/cron"

if [[ -d "$CRON_DIR" ]]; then
    CURRENT_CRONTAB=$(crontab -l 2>/dev/null || true)
    UPDATED_CRONTAB="$CURRENT_CRONTAB"
    CHANGED=false

    for cron_file in "$CRON_DIR"/*; do
        [[ ! -f "$cron_file" ]] && continue

        cron_name=$(basename "$cron_file")
        cron_content=$(cat "$cron_file")
        cron_marker="# jaspah:$cron_name"
        cron_entry="${cron_content} ${cron_marker}"

        if echo "$UPDATED_CRONTAB" | grep -qF "$cron_marker"; then
            existing_line=$(echo "$UPDATED_CRONTAB" | grep -F "$cron_marker")
            if [[ "$existing_line" != "$cron_entry" ]]; then
                echo -e "Updating cron entry: $cron_name\n"
                UPDATED_CRONTAB=$(echo "$UPDATED_CRONTAB" | sed "s|.*${cron_marker}|${cron_entry}|")
                CHANGED=true
            else
                echo -e "Cron entry up to date: $cron_name\n"
            fi
        else
            echo -e "Adding cron entry: $cron_name\n"
            UPDATED_CRONTAB="${UPDATED_CRONTAB}"$'\n'"${cron_entry}"
            CHANGED=true
        fi
    done

    while IFS= read -r cron_line; do
        if [[ "$cron_line" =~ \#\ jaspah:([^[:space:]]+) ]]; then
            cron_name="${BASH_REMATCH[1]}"
            if [[ ! -f "$CRON_DIR/$cron_name" ]]; then
                echo -e "Removing stale cron entry: $cron_name\n"
                UPDATED_CRONTAB=$(echo "$UPDATED_CRONTAB" | grep -vF "$cron_line")
                CHANGED=true
            fi
        fi
    done <<< "$UPDATED_CRONTAB"

    if [[ "$CHANGED" == true ]]; then
        echo "$UPDATED_CRONTAB" | crontab -
    fi
fi
