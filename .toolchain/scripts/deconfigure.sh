#!/usr/bin/env bash
# Remove jaspah-managed live-system configuration without deleting this checkout.

set -euo pipefail

: "${JASPAH_ROOT:?must be set by configure.sh}"

remove_managed_block() {
    local file="$1"
    local prefix="$2"
    local delimiter="${prefix}=== CUSTOM CONFIGURATIONS ==="
    local tmp

    [[ -f "$file" ]] || return 0

    if [[ "$(head -n 1 "$file")" == "$delimiter" ]]; then
        echo "Removing managed block from $file"
        tmp="$(mktemp)"
        sed "1,/^${prefix}=== CUSTOM CONFIGURATIONS ===$/d" "$file" > "$tmp"
        mv "$tmp" "$file"
    fi
}

remove_link_to_root() {
    local path="$1"
    local label="$2"
    local target

    [[ -L "$path" ]] || return 0
    target="$(readlink "$path")"

    if [[ "$target" == "$JASPAH_ROOT"/* ]]; then
        echo "Removing $label: $path"
        rm "$path"
    fi
}

remove_managed_block "$HOME/.bashrc" "#"
remove_managed_block "$HOME/.gitconfig" "#"
remove_managed_block "$HOME/.tmux.conf" "#"

remove_link_to_root "$HOME/.powerline-shell.json" "powerline-shell config symlink"
remove_link_to_root "$HOME/.powerline-shell-theme.py" "powerline-shell theme symlink"
remove_link_to_root "$HOME/.config/herdr/config.toml" "Herdr config symlink"
remove_link_to_root "$HOME/.config/systemd/user/emacs.service" "stale Emacs service symlink"

if command -v uv >/dev/null 2>&1; then
    powerline_segments_dir="$(find "$(uv tool dir)" -path '*powerline*' -type d -name segments 2>/dev/null | sort -V | tail -1)"
    if [[ -n "$powerline_segments_dir" ]]; then
        remove_link_to_root "$powerline_segments_dir/opam_switch.py" "powerline opam_switch segment symlink"
        remove_link_to_root "$powerline_segments_dir/outside.py" "powerline outside segment symlink"
    fi
fi

if command -v crontab >/dev/null 2>&1; then
    current_crontab="$(crontab -l 2>/dev/null || true)"
    if grep -qF '# jaspah:' <<< "$current_crontab"; then
        echo "Removing jaspah-managed cron entries"
        filtered_crontab="$(grep -vF '# jaspah:' <<< "$current_crontab" || true)"
        printf '%s\n' "$filtered_crontab" | crontab -
    fi
fi

jaspah_config_dir="$HOME/.config/jaspah"
if [[ -f "$jaspah_config_dir/user.env" ]]; then
    echo "Removing cached identity: $jaspah_config_dir/user.env"
    rm "$jaspah_config_dir/user.env"
fi
if [[ -d "$jaspah_config_dir" ]]; then
    rmdir "$jaspah_config_dir" 2>/dev/null || true
fi

if command -v nix >/dev/null 2>&1 && nix profile list 2>/dev/null | grep -Fq "Original flake URL: path:$JASPAH_ROOT"; then
    echo "Removing jzp Nix profile entry for $JASPAH_ROOT"
    nix profile remove jzp >/dev/null 2>&1 || true
fi

if [[ "$(uname -s)" == "Linux" ]] && command -v systemctl >/dev/null 2>&1; then
    systemctl --user daemon-reload || true
fi

echo "Deconfigured jaspah-managed system state. Repository left intact at $JASPAH_ROOT"
