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

export JCONFIG_ROOT="$HOME/jconfig"

#=== Aliases ===================================================================
source $JCONFIG_ROOT/aliases/$HOST_OS.sh

#=== Fixes =====================================================================
# C-l fix that keeps current typed command
bind -x $'"C-l":clear;'

#=== Eternal Bash History ======================================================
export HISTFILESIZE=
export HISTSIZE=
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history
# file upon close.
#export HISTFILE=~/config/.bash_eternal_history
# Force prompt to write history after every command.
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

#=== External Sources ==========================================================
if [ "$HOST_OS" == "linux" ]; then
    # For now, the shell prompt setup is only working for linux, so we only want
    # to enable it for a linux host.
    source ~/jconfig/scripts/shell_prompt.sh
fi

source ~/jconfig/scripts/utilities.sh
