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
