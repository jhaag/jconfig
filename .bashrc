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

eval `dircolors ~/.dir_colors/dircolors`

export JCONFIG_ROOT="$HOME/jconfig"
export PATH=$PATH:/usr/local/bin

#=== Aliases ===================================================================
source $JCONFIG_ROOT/aliases/$HOST_OS.sh
source $JCONFIG_ROOT/aliases/shared.sh

#=== External Sources ==========================================================
source ~/jconfig/scripts/shell_prompt.sh
source ~/jconfig/scripts/utilities.sh

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
export HISTFILE=$JCONFIG_ROOT/.bash_eternal_history
# Force prompt to write history after every command.
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
