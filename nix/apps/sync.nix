{ writeShellApplication, bash }:

writeShellApplication {
  name = "jconfig-sync";
  runtimeInputs = [ bash ];
  text = ''
    set -euo pipefail

    if [[ "''${1:-}" == "--help" || "''${1:-}" == "-h" ]]; then
      cat <<'EOF'
    jconfig-sync: run jconfig's .toolchain/sync.sh through a Nix app wrapper

    Usage:
      nix run .#sync
      JCONFIG_ROOT=/path/to/jconfig nix run .#sync

    The wrapper resolves JCONFIG_ROOT from $JCONFIG_ROOT or $PWD.
    EOF
      exit 0
    fi

    root="''${JCONFIG_ROOT:-$PWD}"
    if [[ ! -x "$root/.toolchain/sync.sh" ]]; then
      echo "jconfig-sync: run from jconfig root or set JCONFIG_ROOT" >&2
      exit 1
    fi

    export JCONFIG_ROOT="$root"
    exec "$root/.toolchain/sync.sh" "$@"
  '';
}
