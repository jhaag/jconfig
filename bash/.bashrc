#=== Initialization ============================================================
# Get the host machine, and save it as $HOST_OS; unsupported OSs throw a warning
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

# setup the dircolors to work for solarized
eval `dircolors ~/.dir_colors/dircolors`

export JASPAH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
unset JCONFIG_ROOT JCONFIG_NAME JCONFIG_EMAIL
export EDITOR="emacs -nw"

# Load cached user identity (name/email set during configure.sh)
if [[ -f "$HOME/.config/jaspah/user.env" ]]; then
    source "$HOME/.config/jaspah/user.env"
fi

# Activate dev venv if it exists (sets PATH and VIRTUAL_ENV)
if [[ -f "$HOME/.venv/dev/bin/activate" ]]; then
    source "$HOME/.venv/dev/bin/activate"
fi

# Add source-built emacs to PATH if it exists
if [[ -d "$HOME/.local/emacs/bin" ]]; then
    export PATH="$HOME/.local/emacs/bin:$PATH"
fi
export PATH="$PATH:/usr/local/bin"

# Hook direnv when available. direnv is optional for bootstrap: configure.sh
# must work without it, and the jzp Nix profile is the preferred provider once
# installed. A temporary OS package is only useful for trying direnv before jzp.
if command -v direnv >/dev/null 2>&1; then
    eval "$(direnv hook bash)"
fi

#=== Aliases ===================================================================
source $JASPAH_ROOT/aliases/$HOST_OS.sh
source $JASPAH_ROOT/aliases/shared.sh

#=== External Sources ==========================================================
source $JASPAH_ROOT/scripts/shell_prompt.sh
source $JASPAH_ROOT/scripts/utilities.sh
source $JASPAH_ROOT/bash/.git-completion.bash

#=== Fixes =====================================================================
if is_interactive_shell; then
    # C-l fix that keeps current typed command
    bind -x $'"C-l":clear;'
fi

#=== Eternal Bash History ======================================================
export HISTFILESIZE=
export HISTSIZE=
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history
# file upon close.
export HISTFILE=$JASPAH_ROOT/.bash_eternal_history
# Force prompt to write history after every command (as the final step).
PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
