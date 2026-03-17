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

# Download emacs source tarball to cache directory.
# $1: version string (e.g., "29.4")
# Returns 1 on failure.
emacs_download_source() {
    local version="$1"
    local tarball="emacs-${version}.tar.gz"
    local url="https://ftp.gnu.org/gnu/emacs/${tarball}"
    local dest="$EMACS_BUILD_CACHE/$tarball"

    mkdir -p "$EMACS_BUILD_CACHE"

    if [[ -f "$dest" ]]; then
        echo "Source tarball already cached: $tarball"
        return 0
    fi

    echo "Downloading $tarball..."
    if ! curl -fSL --progress-bar -o "$dest" "$url"; then
        echo "ERROR: Failed to download $url"
        rm -f "$dest"
        return 1
    fi

    # GPG verification if available
    if command -v gpg >/dev/null 2>&1; then
        local sig_url="${url}.sig"
        local sig_dest="${dest}.sig"
        echo "Downloading GPG signature..."
        if curl -fsSL -o "$sig_dest" "$sig_url" 2>/dev/null; then
            # Import GNU keyring if not already present
            gpg --keyserver keyserver.ubuntu.com --recv-keys 17E90D521672C04631B1183EE78DAE0F3115E06B 2>/dev/null
            if gpg --verify "$sig_dest" "$dest" 2>/dev/null; then
                echo "GPG signature verified."
            else
                echo "WARNING: GPG signature verification failed. Proceeding anyway."
            fi
        else
            echo "WARNING: Could not download GPG signature. Skipping verification."
        fi
    else
        echo "WARNING: gpg not installed. Skipping tarball signature verification."
    fi

    return 0
}

# Extract source tarball, wiping any prior extract.
# $1: version string
emacs_extract_source() {
    local version="$1"
    local tarball="$EMACS_BUILD_CACHE/emacs-${version}.tar.gz"
    local src_dir="$EMACS_BUILD_CACHE/emacs-${version}"

    if [[ -d "$src_dir" ]]; then
        echo "Removing previous source directory..."
        rm -rf "$src_dir"
    fi

    echo "Extracting emacs-${version}.tar.gz..."
    tar -xzf "$tarball" -C "$EMACS_BUILD_CACHE"
}

# Run ./configure with user's flags.
# $1: version string
# Returns 1 on failure (missing deps, etc.)
emacs_configure_source() {
    local version="$1"
    local src_dir="$EMACS_BUILD_CACHE/emacs-${version}"

    echo -e "\nConfiguring emacs $version..."
    echo "  prefix: $EMACS_INSTALL_PREFIX"
    [[ -n "$JCONFIG_EMACS_CONFIGURE_FLAGS" ]] && echo "  flags: $JCONFIG_EMACS_CONFIGURE_FLAGS"

    # Run in subshell to avoid polluting the parent shell's working directory
    # shellcheck disable=SC2086
    if ! (cd "$src_dir" && ./configure --prefix="$EMACS_INSTALL_PREFIX" $JCONFIG_EMACS_CONFIGURE_FLAGS); then
        echo ""
        echo "ERROR: ./configure failed. This usually means build dependencies are missing."
        echo "Review the output above for details on which packages are needed."
        return 1
    fi

    return 0
}

# Build emacs from configured source.
# $1: version string
emacs_build_source() {
    local version="$1"
    local src_dir="$EMACS_BUILD_CACHE/emacs-${version}"
    local nproc

    if [[ "$(uname -s)" == "Darwin" ]]; then
        nproc=$(sysctl -n hw.ncpu)
    else
        nproc=$(nproc)
    fi

    echo -e "\nBuilding emacs $version with $nproc parallel jobs..."

    if ! make -C "$src_dir" -j"$nproc"; then
        echo "ERROR: Build failed."
        return 1
    fi

    return 0
}

# Install emacs to prefix, wiping old install first.
# $1: version string
emacs_install_source() {
    local version="$1"
    local src_dir="$EMACS_BUILD_CACHE/emacs-${version}"

    # Wipe old install to prevent stale files
    if [[ -d "$EMACS_INSTALL_PREFIX" ]]; then
        echo "Removing previous installation at $EMACS_INSTALL_PREFIX..."
        rm -rf "$EMACS_INSTALL_PREFIX"
    fi

    echo "Installing emacs $version to $EMACS_INSTALL_PREFIX..."

    if ! make -C "$src_dir" install; then
        echo "ERROR: make install failed."
        return 1
    fi

    echo -e "Emacs $version installed successfully.\n"
    return 0
}

# Full build pipeline: download, extract, configure, build, install.
# $1: version string
# Returns 1 on any failure.
emacs_full_build() {
    local version="$1"

    emacs_download_source "$version" || return 1
    emacs_extract_source "$version" || return 1
    emacs_configure_source "$version" || return 1
    emacs_build_source "$version" || return 1
    emacs_install_source "$version" || return 1

    return 0
}

# Main entry point for emacs build management.
# Called from configure.sh.
emacs_build_main() {
    echo -e "\n#=== Emacs Source Build =========================================================\n"

    local managed_emacs="$EMACS_INSTALL_PREFIX/bin/emacs"
    local managed_version=""
    local system_emacs=""
    local system_version=""
    local needs_build=false
    local target_version=""

    # Detect managed install
    if [[ -x "$managed_emacs" ]]; then
        managed_version=$(emacs_get_binary_version "$managed_emacs")
    fi

    # Detect system install
    if [[ -z "$managed_version" ]]; then
        system_emacs=$(command -v emacs 2>/dev/null || true)
        if [[ -n "$system_emacs" ]]; then
            system_version=$(emacs_get_binary_version "$system_emacs")
        fi
    fi

    # Load or prompt for settings
    if emacs_load_settings; then
        target_version="$JCONFIG_EMACS_VERSION"
    else
        # No settings file -- first run
        echo "No emacs build configuration found."

        # Case B: system emacs exists
        if [[ -n "$system_version" ]]; then
            echo ""
            echo "Found emacs $system_version at $system_emacs (system-managed)."
            echo "This script will build emacs from source and install to $EMACS_INSTALL_PREFIX."
            echo "The system package will remain but $EMACS_INSTALL_PREFIX/bin/ will take PATH priority."
            echo ""
            local proceed
            read -r -p "Proceed? (y/n) [y]: " proceed
            if [[ "${proceed:-y}" =~ ^[Nn] ]]; then
                echo -e "Skipping emacs build.\n"
                return 0
            fi
        fi

        # Resolve latest for default prompt value
        echo ""
        echo "Resolving latest emacs version..."
        local latest
        latest=$(emacs_resolve_latest_version)
        if [[ -z "$latest" ]]; then
            echo "WARNING: Could not resolve latest version. You must specify a version manually."
            latest=""
        else
            echo "Latest available: $latest"
        fi

        emacs_prompt_settings "$latest" || return 1
        emacs_save_settings
        target_version="$JCONFIG_EMACS_VERSION"
        needs_build=true
    fi

    # Auto-update check (only if we have settings and didn't just prompt)
    if [[ "$needs_build" == "false" ]]; then
        local latest
        latest=$(emacs_resolve_latest_version)

        if [[ -n "$latest" ]] && emacs_auto_update_allows "$target_version" "$latest"; then
            echo "Auto-update: $target_version -> $latest"
            target_version="$latest"
            JCONFIG_EMACS_VERSION="$target_version"
            emacs_save_settings
            needs_build=true
        elif [[ -z "$latest" ]]; then
            echo "WARNING: Could not check for updates. Using configured version $target_version."
        fi
    fi

    # Case A: managed install exists, check version match
    if [[ -n "$managed_version" && "$needs_build" == "false" ]]; then
        local cmp
        cmp=$(emacs_compare_versions "$target_version" "$managed_version")

        if [[ "$cmp" == "equal" ]]; then
            echo "Emacs $managed_version already installed at $EMACS_INSTALL_PREFIX."
            return 0
        fi

        # Version mismatch, not resolved by auto-update
        echo ""
        echo "Version mismatch: configured $target_version, installed $managed_version."
        local rebuild
        read -r -p "Rebuild to $target_version? (y/n) [y]: " rebuild
        if [[ "${rebuild:-y}" =~ ^[Nn] ]]; then
            echo -e "Skipping emacs build.\n"
            return 0
        fi
        needs_build=true
    fi

    # No managed install and we have settings = first build or system-only
    if [[ -z "$managed_version" && "$needs_build" == "false" ]]; then
        needs_build=true
    fi

    # Build
    if [[ "$needs_build" == "true" ]]; then
        emacs_full_build "$target_version" || return 1

        # Update systemd service on Linux
        if [[ "$HOST_OS" == "linux" ]]; then
            emacs_update_systemd_service
        fi
    fi
}

# Update systemd service to point to managed emacs and restart.
emacs_update_systemd_service() {
    local was_running=false

    if systemctl --user is-active emacs.service &>/dev/null; then
        was_running=true
    fi

    systemctl --user daemon-reload

    if ! systemctl --user is-enabled emacs.service &>/dev/null; then
        systemctl --user enable emacs.service
        echo "Enabled emacs daemon service."
    fi

    if [[ "$was_running" == "true" ]]; then
        echo "Restarting emacs daemon..."
        systemctl --user restart emacs.service
    else
        echo "Starting emacs daemon..."
        systemctl --user start emacs.service
    fi
}
