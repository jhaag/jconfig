#=== Aliases ===================================================================
#--- ls aliases ----------------------------------------------------------------
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

#=== Fixes =====================================================================
# C-l fix that keeps current typed command
bind -x $'"C-l":clear;'

#=== Eternal Bash History ======================================================
export HISTFILESIZE=-1
export HISTSIZE=-1
export HISTCONTROL=ignoreboth
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history
# file upon close.
#export HISTFILE=~/config/.bash_eternal_history
# Force prompt to write history after every command.
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

#=== External Sources ==========================================================
source ~/jconfig/scripts/shell_prompt.sh
source ~/jconfig/scripts/utilities.sh
