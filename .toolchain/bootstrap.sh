#!/usr/bin/env bash
# Interactive one-time prereqs: uv, Solarized terminal theme (Linux), user identity
# cached to ~/.config/jaspah/user.env. Each block self-gates; re-runs are a no-op
# on a configured machine.

: "${JASPAH_ROOT:?must be set by configure.sh}"

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     HOST_OS=linux ;;
    Darwin*)    HOST_OS=darwin ;;
    *)          HOST_OS="UNKNOWN:${unameOut}"
                echo "WARNING: Dotfile configuration not supported for ${HOST_OS}"
                ;;
esac

#--- Solarized Dark Terminal Setup ---------------------------------------------
if [ "$HOST_OS" == "linux" ]; then
    if [[ ! -e ~/gnome-terminal-colors-solarized/ ]]; then
        pushd "$HOME" >/dev/null
        git clone https://github.com/sigurdga/gnome-terminal-colors-solarized.git
        cd gnome-terminal-colors-solarized
        ./set_dark.sh
        popd >/dev/null

        echo '[NOTE] If solarized dark is not the theme for the terminal, look into it <https://github.com/seebi/dircolors-solarized>'
        echo 'try using "eval `dircolors ~/.dir_color`"'
    fi
fi

#--- uv ------------------------------------------------------------------------
if ! command -v uv >/dev/null 2>&1; then
    echo -e "Installing uv package manager...\n"
    curl -LsSf https://astral.sh/uv/install.sh | sh
fi

#--- User Identity -------------------------------------------------------------
JASPAH_USER_ENV="$HOME/.config/jaspah/user.env"

if [[ -f "$JASPAH_USER_ENV" ]]; then
    source "$JASPAH_USER_ENV"
fi

prompted=false
if [[ -z "$JASPAH_NAME" ]]; then
    read -r -p "Enter your full name: " JASPAH_NAME
    prompted=true
fi
if [[ -z "$JASPAH_EMAIL" ]]; then
    read -r -p "Enter your email address: " JASPAH_EMAIL
    prompted=true
fi

if [[ "$prompted" == "true" ]]; then
    mkdir -p "$(dirname "$JASPAH_USER_ENV")"
    cat > "$JASPAH_USER_ENV" <<EOF
export JASPAH_NAME="$JASPAH_NAME"
export JASPAH_EMAIL="$JASPAH_EMAIL"
EOF
    echo -e "User identity cached at $JASPAH_USER_ENV\n"
fi
