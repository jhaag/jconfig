#!/usr/bin/env bash

# TODO: determine whether I want to set this
# set -euo pipefail

# Shim for building macros used by tmux

###| Utilities |####################################################################################
# Helper to print errors to stderr
#
# TODO: inject error into tmux and display in pop-up instead of using stderr
log_error() {
    printf "\e[31m[Error]\e[0m %s\n" "$1" >&2
}
####################################################################################################

###| Curryable Functions |##########################################################################
__jconfig_tmux_bracket() {
    if (( $# != 3 )); then
        log_error "'__jconfig_tmux_bracket' expects 3 (ordered) args: L_bracket R_bracket Content"
        exit 1
    fi
    echo "$1$3$2"
}
####################################################################################################

###| Dispatcher |###################################################################################
dispatch() {
    local method_name="$1"
    shift # The rest are arguments

    # Check if the string "looks like" a tmux macro function
    if [[ "$method_name" != "__jconfig_tmux"* ]]; then
        log_error "Method doesn't start with '__jconfig_tmux': '$method_name'"
        exit 1
    fi

    # Check if the string is actually a defined function
    if [[ $(type -t "$method_name") == "function" ]]; then
        "$method_name" "$@"
    else
        log_error "Unknown method: '$method_name'"
        echo "Usage: $0 <method_name> [args...]"
        exit 1
    fi
}
####################################################################################################

###| Entrypoint |###################################################################################
if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <method_name> [argstring]"
    exit 1
fi
dispatch "$@"
####################################################################################################
