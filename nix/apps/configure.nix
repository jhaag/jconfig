{ writeShellApplication, bash }:

writeShellApplication {
  name = "jaspah-configure";
  runtimeInputs = [ bash ];
  text = ''
    set -euo pipefail

    if [[ "''${1:-}" == "--help" || "''${1:-}" == "-h" ]]; then
      cat <<'EOF'
    jaspah-configure: run jaspah's configure.sh through a Nix app wrapper

    Usage:
      nix run .#configure
      JASPAH_ROOT=/path/to/jaspah nix run .#configure

    The wrapper resolves JASPAH_ROOT from $JASPAH_ROOT or $PWD.
    EOF
      exit 0
    fi

    root="''${JASPAH_ROOT:-$PWD}"
    if [[ ! -x "$root/configure.sh" ]]; then
      echo "jaspah-configure: run from jaspah root or set JASPAH_ROOT" >&2
      exit 1
    fi

    exec "$root/configure.sh" "$@"
  '';
}
