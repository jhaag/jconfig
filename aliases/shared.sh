function cdl {
    cd "$@" && ll
}

# Move export GREP_OPTIONS="--color=auto" (which is deprecated) from .exports to .alias
# Always enable colored `grep` output`
unset GREP_OPTIONS
alias grep="/usr/bin/grep --color=auto"
alias fgrep="fgrep --color=auto"
alias egrep="egrep --color=auto"
