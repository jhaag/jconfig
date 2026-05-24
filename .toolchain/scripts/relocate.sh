#!/usr/bin/env bash
# Relocate a pre-rename checkout and live system state to the current jaspah layout.
#
# Usage:
#   .toolchain/scripts/relocate.sh [new-local-clone-name] [--origin <git-url>]
#
# Examples:
#   .toolchain/scripts/relocate.sh jaspah --origin git@github.com:jhaag/jaspah.git
#   .toolchain/scripts/relocate.sh --origin git@github.com:jhaag/jaspah.git

set -euo pipefail

usage() {
    sed -n '2,9p' "$0" | sed 's/^# \{0,1\}//'
}

origin_url=""
new_name=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --origin)
            if [[ $# -lt 2 || -z "${2:-}" ]]; then
                echo "relocate.sh: --origin requires a git URL" >&2
                exit 2
            fi
            origin_url="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        --)
            shift
            break
            ;;
        -*)
            echo "relocate.sh: unknown option: $1" >&2
            usage >&2
            exit 2
            ;;
        *)
            if [[ -n "$new_name" ]]; then
                echo "relocate.sh: only one new local clone name is supported" >&2
                usage >&2
                exit 2
            fi
            new_name="${1%/}"
            shift
            ;;
    esac
done

if [[ $# -gt 0 ]]; then
    if [[ -n "$new_name" ]]; then
        echo "relocate.sh: only one new local clone name is supported" >&2
        usage >&2
        exit 2
    fi
    new_name="${1%/}"
    shift
    if [[ $# -gt 0 ]]; then
        echo "relocate.sh: only one new local clone name is supported" >&2
        usage >&2
        exit 2
    fi
fi

if [[ -z "$new_name" && -z "$origin_url" ]]; then
    echo "relocate.sh: provide a new local clone name and/or --origin <git-url>" >&2
    usage >&2
    exit 2
fi

if [[ -n "$new_name" && "$new_name" == */* ]]; then
    echo "relocate.sh: new local clone name must be a directory name, not a path: $new_name" >&2
    exit 2
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$script_dir/../.." && pwd)"
parent="$(dirname "$root")"
target_root="$root"

if [[ -n "$new_name" ]]; then
    target_root="$parent/$new_name"
    if [[ "$target_root" != "$root" ]]; then
        if [[ -e "$target_root" ]]; then
            echo "relocate.sh: target already exists: $target_root" >&2
            exit 1
        fi

        echo "Renaming checkout: $root -> $target_root"
        mv "$root" "$target_root"
        root="$target_root"
    fi
fi

if [[ -n "$origin_url" ]]; then
    if git -C "$root" remote get-url origin >/dev/null 2>&1; then
        echo "Setting origin URL: $origin_url"
        git -C "$root" remote set-url origin "$origin_url"
    else
        echo "Adding origin URL: $origin_url"
        git -C "$root" remote add origin "$origin_url"
    fi
fi

target_name="$(basename "$root")"

# Patch live files that are generated outside configure.sh's managed include
# blocks. The Emacs config is tangled manually, so configure.sh does not own it.
if [[ -f "$HOME/.emacs" ]]; then
    echo "Updating live ~/.emacs path references"
    tmp_emacs="$(mktemp)"
    sed \
        -e "s|$HOME/jconfig|$HOME/$target_name|g" \
        -e "s|~/jconfig|~/$target_name|g" \
        -e "s|\$HOME/jconfig|\$HOME/$target_name|g" \
        "$HOME/.emacs" > "$tmp_emacs"
    mv "$tmp_emacs" "$HOME/.emacs"
fi

# Migrate the old machine-local identity cache. This one-time relocation script is
# the only place that knows about the previous jconfig namespace.
old_config_dir="$HOME/.config/jconfig"
new_config_dir="$HOME/.config/jaspah"
old_user_env="$old_config_dir/user.env"
new_user_env="$new_config_dir/user.env"

if [[ -f "$old_user_env" && ! -f "$new_user_env" ]]; then
    # shellcheck disable=SC1090
    source "$old_user_env"
    mkdir -p "$new_config_dir"
    cat > "$new_user_env" <<EOF
export JASPAH_NAME="${JASPAH_NAME:-${JCONFIG_NAME:-}}"
export JASPAH_EMAIL="${JASPAH_EMAIL:-${JCONFIG_EMAIL:-}}"
EOF
    echo "Migrated user identity cache: $old_user_env -> $new_user_env"
fi

if [[ -d "$old_config_dir" ]]; then
    unknown_old_config="$(find "$old_config_dir" -mindepth 1 -maxdepth 1 \
        ! -name user.env \
        ! -name emacs.env \
        ! -name emacs-configure.sh \
        -print -quit)"
    if [[ -z "$unknown_old_config" ]]; then
        echo "Removing obsolete machine-local config dir: $old_config_dir"
        rm -rf "$old_config_dir"
    else
        echo "Leaving $old_config_dir in place; it contains unrecognized files." >&2
    fi
fi

# Drop old managed cron entries. configure.sh/sync.sh will add current jaspah
# entries immediately below.
if command -v crontab >/dev/null 2>&1; then
    current_crontab="$(crontab -l 2>/dev/null || true)"
    if grep -qF '# jconfig:' <<< "$current_crontab"; then
        echo "Removing old jconfig-managed cron entries"
        grep -vF '# jconfig:' <<< "$current_crontab" | crontab -
    fi
fi

# If the old profile points at the pre-rename checkout path, replace it with the
# current checkout path.
if command -v nix >/dev/null 2>&1 && nix profile list 2>/dev/null | grep -Eq 'Original flake URL: path:.*/jconfig($|[/?])'; then
    echo "Reinstalling jzp Nix profile from path:$root#jzp"
    nix profile remove jzp >/dev/null 2>&1 || true
    nix profile add "path:$root#jzp"
fi

# The Emacs source-build/service manager was removed. If the old repo left behind
# the managed user service symlink, remove it instead of carrying it forward.
emacs_service="$HOME/.config/systemd/user/emacs.service"
if [[ -L "$emacs_service" ]]; then
    emacs_target="$(readlink "$emacs_service")"
    if [[ "$emacs_target" == *"/jconfig/systemd/emacs.service" || "$emacs_target" == *"/jaspah/systemd/emacs.service" ]]; then
        echo "Removing stale managed Emacs service symlink: $emacs_service"
        rm "$emacs_service"
        if [[ "$(uname -s)" == "Linux" ]] && command -v systemctl >/dev/null 2>&1; then
            systemctl --user daemon-reload || true
        fi
    fi
fi

echo "Running configure.sh from $root"
JASPAH_ROOT="$root" "$root/configure.sh"

echo "Relocation complete. Open a new shell or run: source ~/.bashrc"
