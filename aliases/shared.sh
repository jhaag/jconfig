function cdl {
    cd "$@" && ll
}

# cht.sh is a cheat sheet for various unix commands and their general usage. You need to
# be online, but whateber.
function cheat() {
    curl cht.sh/$1
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
