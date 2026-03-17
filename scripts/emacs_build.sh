#!/usr/bin/env bash
# Emacs source build management for jconfig
# Sourced by configure.sh -- do not execute directly.

EMACS_INSTALL_PREFIX="$HOME/.local/emacs"
EMACS_BUILD_CACHE="$HOME/.cache/jconfig/emacs-build"
EMACS_ENV_FILE="$HOME/.config/jconfig/emacs.env"

# Resolve the latest stable emacs version from GNU FTP.
# Falls back to GitHub API if FTP is unreachable.
# Prints the version string (e.g., "29.4") to stdout.
# Returns 1 if both sources fail.
emacs_resolve_latest_version() {
    local version

    # Try GNU FTP first
    version=$(curl -fsSL --max-time 10 "https://ftp.gnu.org/gnu/emacs/" 2>/dev/null \
        | grep -oE 'emacs-[0-9]+\.[0-9]+\.tar\.gz' \
        | sed 's/emacs-//;s/\.tar\.gz//' \
        | sort -t. -k1,1n -k2,2n \
        | tail -1)

    if [[ -n "$version" ]]; then
        echo "$version"
        return 0
    fi

    # Fallback: GitHub API
    version=$(curl -fsSL --max-time 10 "https://api.github.com/repos/emacs-mirror/emacs/tags?per_page=100" 2>/dev/null \
        | grep -oE '"name": *"emacs-[0-9]+\.[0-9]+"' \
        | grep -oE '[0-9]+\.[0-9]+' \
        | sort -t. -k1,1n -k2,2n \
        | tail -1)

    if [[ -n "$version" ]]; then
        echo "$version"
        return 0
    fi

    return 1
}

# Get the version of an emacs binary.
# $1: path to emacs binary
# Prints version string (e.g., "29.4") to stdout.
emacs_get_binary_version() {
    local binary="$1"
    "$binary" --version 2>/dev/null | head -1 | grep -oE '[0-9]+\.[0-9]+'
}
