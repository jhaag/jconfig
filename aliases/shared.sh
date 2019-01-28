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

# bc I'm lazy
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
