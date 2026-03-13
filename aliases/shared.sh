function cdl {
    cd "$@" && ll
}

# cht.sh is a cheat sheet for various unix commands and their general usage. You need to
# be online, but whateber.
function cheat {
    curl cht.sh/$1
}

function venv-activate {
    if [[ -z $1 ]]; then
        echo "Usage: venv-activate <VENV NAME>"
        return 1
    fi

    local venv_base_dir="$HOME/.venv"
    local venv_dir="$venv_base_dir/$1"

    if [[ ! -d "${venv_base_dir}" ]]; then
        echo "${venv_base_dir} should store the python virtual environments (or it should symlink the actual venv store)"
        return 1
    fi

    if [[ ! -d "${venv_dir}" ]]; then
        echo "${venv_base_dir} exists, but ${1} doesn't appear to be a venv; here are the available venvs:"
        ll "${venv_base_dir}"
        return 1
    fi

    source "${venv_dir}/bin/activate"
}

function venv-deactivate {
    if [[ ! -d "$VIRTUAL_ENV" ]]; then
        echo "It appears that no python venv is active."
        return 1
    fi

    source "$VIRTUAL_ENV/bin/deactivate"
}

# Quick way to jump into my tmux session from a raw terminal.
# Usage: jmux [path|session-name]
#   If a valid directory is given, uses its basename as session name
#   and cds to it in the second dev pane.
function jmux {
    local adjs=(bold brave calm cool dark dawn deep dusk fair fast
                firm free gold keen lean mild new old pale pure
                quiet rare rich sharp slim soft still swift tall
                warm wide wild wise)
    local nouns=(ash bay birch cedar cliff cove creek dale dawn
                 dell dune fern field fjord flint ford forge glen
                 grove hill lake lark mast moor path peak pine
                 pond reef ridge rock shore slate stone vale wave)
    local adj="${adjs[RANDOM % ${#adjs[@]}]}"
    local noun="${nouns[RANDOM % ${#nouns[@]}]}"
    local random_session="${adj}-${noun}"

    local no_split=0
    local args=()
    for a in "$@"; do
        [[ "$a" == "-1" ]] && no_split=1 || args+=("$a")
    done

    local arg="${args[0]:-}"
    local session dev_path
    if [[ -n "$arg" && -d "$arg" ]]; then
        dev_path="$(realpath "$arg")"
        session="$(basename "$dev_path")"
    elif [[ -n "$arg" ]]; then
        session="$arg"
        dev_path=""
    else
        session="$random_session"
        dev_path=""
    fi

    if [[ "$TERM" =~ ^tmux && -n "$TMUX" ]]; then
        if tmux has-session -t "$session" 2>/dev/null; then
            tmux switch-client -t "$session"
            exit
        fi
        local term cmd
        cmd="jmux $(printf '%q ' "$@")"
        for term in "$TERMINAL" x-terminal-emulator xterm; do
            [[ -n "$term" ]] && command -v "$term" &>/dev/null || continue
            nohup "$term" -e bash -i -c "$cmd" &>/dev/null &
            return
        done
        echo "jmux: no terminal emulator found (set \$TERMINAL)" >&2
        return 1
    else
        export SESSION_PATH="$dev_path"
        [[ $no_split -eq 1 ]] && export SESSION_NO_SPLIT=1
        source ~/jconfig/scripts/tmux_startup.sh "$session"
        unset SESSION_PATH SESSION_NO_SPLIT
    fi
}

function _jmux_complete {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=($(compgen -d -- "$cur"))
}
complete -o nospace -o filenames -F _jmux_complete jmux

function tmux-notes {
    if ! [[ "$TERM" =~ ^tmux && -n "$TMUX" ]]; then
        tmux display-message -p "#{E:@__ALL_NOTES}"
    fi
}

# Move export GREP_OPTIONS="--color=auto" (which is deprecated) from .exports to .alias
# Always enable colored `grep` output`
unset GREP_OPTIONS
alias grep="grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"

# Remove any local branches which aren't being tracked remotely.
alias git-brprune='git fetch --prune && git branch -r | awk "{print \$1}" | egrep -v -f /dev/fd/0 <(git branch -vv | grep origin) | awk "{print \$1}" | xargs git branch -d'

# bc I'm lazy
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

# Make emacs default to terminal mode
emacs() {
    command emacs -nw "$@"
}

# GUI version of emacs
gemacs() {
    command emacs "$@"
}
