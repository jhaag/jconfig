#!/usr/bin/env bash
# Self-repairing entry shim. See .toolchain/AGENTS.md.

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  ./configure.sh
  ./configure.sh --change-name <new-local-clone-name> [--change-origin <git-url>]
  ./configure.sh --change-origin <git-url> [--change-name <new-local-clone-name>]
  ./configure.sh --deconfigure

Options:
  --change-name <name>      Rename this checkout within its parent directory, then configure.
  --change-origin <giturl>  Set/add the git origin URL, then configure.
  --deconfigure            Remove jaspah-managed live-system config; keep the repo.
  -h, --help               Show this help.
EOF
}

JASPAH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export JASPAH_ROOT

change_name=""
change_origin=""
deconfigure=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        --change-name)
            if [[ $# -lt 2 || -z "${2:-}" ]]; then
                echo "configure.sh: --change-name requires a directory name" >&2
                exit 2
            fi
            change_name="${2%/}"
            shift 2
            ;;
        --change-origin)
            if [[ $# -lt 2 || -z "${2:-}" ]]; then
                echo "configure.sh: --change-origin requires a git URL" >&2
                exit 2
            fi
            change_origin="$2"
            shift 2
            ;;
        --deconfigure)
            deconfigure=true
            shift
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "configure.sh: unknown argument: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

if [[ "$deconfigure" == true ]]; then
    if [[ -n "$change_name" || -n "$change_origin" ]]; then
        echo "configure.sh: --deconfigure cannot be combined with --change-name/--change-origin" >&2
        exit 2
    fi
    exec "$JASPAH_ROOT/.toolchain/scripts/deconfigure.sh"
fi

if [[ -n "$change_name" || -n "$change_origin" ]]; then
    relocate_args=()
    [[ -n "$change_name" ]] && relocate_args+=("$change_name")
    [[ -n "$change_origin" ]] && relocate_args+=(--origin "$change_origin")
    exec "$JASPAH_ROOT/.toolchain/scripts/relocate.sh" "${relocate_args[@]}"
fi

"$JASPAH_ROOT/.toolchain/bootstrap.sh"
"$JASPAH_ROOT/.toolchain/sync.sh"
