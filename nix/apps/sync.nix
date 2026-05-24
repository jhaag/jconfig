{ writeShellApplication, bash }:

writeShellApplication {
  name = "jaspah-sync";
  runtimeInputs = [ bash ];
  text = ''
    set -euo pipefail

    if [[ "''${1:-}" == "--help" || "''${1:-}" == "-h" ]]; then
      cat <<'EOF'
    jaspah-sync: run jaspah's .toolchain/sync.sh through a Nix app wrapper

    Usage:
      nix run .#sync
      JASPAH_ROOT=/path/to/jaspah nix run .#sync

    The wrapper resolves JASPAH_ROOT from $JASPAH_ROOT or $PWD.
    EOF
      exit 0
    fi

    root="''${JASPAH_ROOT:-$PWD}"
    if [[ ! -x "$root/.toolchain/sync.sh" ]]; then
      echo "jaspah-sync: run from jaspah root or set JASPAH_ROOT" >&2
      exit 1
    fi

    export JASPAH_ROOT="$root"
    exec "$root/.toolchain/sync.sh" "$@"
  '';
}
