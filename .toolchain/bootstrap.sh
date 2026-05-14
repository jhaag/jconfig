#!/usr/bin/env bash
# Interactive one-time prereqs: uv, Solarized terminal theme (Linux), user identity
# cached to ~/.config/jconfig/user.env. Each block self-gates; re-runs are a no-op
# on a configured machine.

: "${JCONFIG_ROOT:?must be set by configure.sh}"

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
JCONFIG_USER_ENV="$HOME/.config/jconfig/user.env"

if [[ -f "$JCONFIG_USER_ENV" ]]; then
    source "$JCONFIG_USER_ENV"
fi

prompted=false
if [[ -z "$JCONFIG_NAME" ]]; then
    read -r -p "Enter your full name: " JCONFIG_NAME
    prompted=true
fi
if [[ -z "$JCONFIG_EMAIL" ]]; then
    read -r -p "Enter your email address: " JCONFIG_EMAIL
    prompted=true
fi

if [[ "$prompted" == "true" ]]; then
    mkdir -p "$(dirname "$JCONFIG_USER_ENV")"
    cat > "$JCONFIG_USER_ENV" <<EOF
export JCONFIG_NAME="$JCONFIG_NAME"
export JCONFIG_EMAIL="$JCONFIG_EMAIL"
EOF
    echo -e "User identity cached at $JCONFIG_USER_ENV\n"
fi
