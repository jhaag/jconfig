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

# Load emacs build settings from env file.
# Sets JCONFIG_EMACS_VERSION, JCONFIG_EMACS_AUTO_UPDATE_MAJOR,
# JCONFIG_EMACS_AUTO_UPDATE_MINOR, JCONFIG_EMACS_CONFIGURE_FLAGS.
emacs_load_settings() {
    if [[ -f "$EMACS_ENV_FILE" ]]; then
        source "$EMACS_ENV_FILE"
        return 0
    fi
    return 1
}

# Save emacs build settings to env file.
emacs_save_settings() {
    mkdir -p "$(dirname "$EMACS_ENV_FILE")"
    cat > "$EMACS_ENV_FILE" <<EOF
JCONFIG_EMACS_VERSION="$JCONFIG_EMACS_VERSION"
JCONFIG_EMACS_AUTO_UPDATE_MAJOR=$JCONFIG_EMACS_AUTO_UPDATE_MAJOR
JCONFIG_EMACS_AUTO_UPDATE_MINOR=$JCONFIG_EMACS_AUTO_UPDATE_MINOR
JCONFIG_EMACS_CONFIGURE_FLAGS="$JCONFIG_EMACS_CONFIGURE_FLAGS"
EOF
    echo -e "Emacs build settings saved to $EMACS_ENV_FILE\n"
}

# Prompt user for emacs build settings.
# $1: default version (typically the resolved latest)
emacs_prompt_settings() {
    local default_version="${1:-latest}"

    read -r -p "Enter desired emacs version (or 'latest' for newest release) [$default_version]: " JCONFIG_EMACS_VERSION
    JCONFIG_EMACS_VERSION="${JCONFIG_EMACS_VERSION:-$default_version}"

    # If user entered 'latest', resolve it now
    if [[ "$JCONFIG_EMACS_VERSION" == "latest" ]]; then
        echo "Resolving latest emacs version..."
        JCONFIG_EMACS_VERSION=$(emacs_resolve_latest_version)
        if [[ -z "$JCONFIG_EMACS_VERSION" ]]; then
            echo "ERROR: Could not resolve latest emacs version. Check your network connection."
            return 1
        fi
        echo "Latest version: $JCONFIG_EMACS_VERSION"
    fi

    local auto_major
    read -r -p "Auto-update major versions? (y/n) [y]: " auto_major
    JCONFIG_EMACS_AUTO_UPDATE_MAJOR=$( [[ "${auto_major:-y}" =~ ^[Yy] ]] && echo true || echo false )

    local auto_minor
    read -r -p "Auto-update minor versions? (y/n) [y]: " auto_minor
    JCONFIG_EMACS_AUTO_UPDATE_MINOR=$( [[ "${auto_minor:-y}" =~ ^[Yy] ]] && echo true || echo false )

    read -r -p "Additional configure flags (e.g., --with-xwidgets) []: " JCONFIG_EMACS_CONFIGURE_FLAGS
    JCONFIG_EMACS_CONFIGURE_FLAGS="${JCONFIG_EMACS_CONFIGURE_FLAGS:-}"
}

# Compare two semver strings. Prints "newer", "older", or "equal".
# $1: version a, $2: version b
emacs_compare_versions() {
    local a_major a_minor b_major b_minor
    a_major=$(echo "$1" | cut -d. -f1)
    a_minor=$(echo "$1" | cut -d. -f2)
    b_major=$(echo "$2" | cut -d. -f1)
    b_minor=$(echo "$2" | cut -d. -f2)

    if (( a_major > b_major )); then echo "newer"; return; fi
    if (( a_major < b_major )); then echo "older"; return; fi
    if (( a_minor > b_minor )); then echo "newer"; return; fi
    if (( a_minor < b_minor )); then echo "older"; return; fi
    echo "equal"
}

# Check if auto-update policy allows upgrading from $1 to $2.
# Returns 0 if upgrade is allowed, 1 otherwise.
emacs_auto_update_allows() {
    local current="$1" candidate="$2"
    local current_major candidate_major

    current_major=$(echo "$current" | cut -d. -f1)
    candidate_major=$(echo "$candidate" | cut -d. -f1)

    # Check if candidate is actually newer
    if [[ "$(emacs_compare_versions "$candidate" "$current")" != "newer" ]]; then
        return 1
    fi

    # Major version change?
    if (( candidate_major != current_major )); then
        [[ "$JCONFIG_EMACS_AUTO_UPDATE_MAJOR" == "true" ]]
        return $?
    fi

    # Minor version change within same major
    [[ "$JCONFIG_EMACS_AUTO_UPDATE_MINOR" == "true" ]]
    return $?
}
