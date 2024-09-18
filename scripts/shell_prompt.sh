function _update_ps1() {
    PS1=$(powerline-shell $?)
}

if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
    if [[ -z "$PROMPT_COMMAND" ]]; then
        PROMPT_COMMAND="_update_ps1"
    else
        PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
    fi
fi
