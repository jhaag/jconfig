{ writeShellApplication, bash }:

writeShellApplication {
  name = "jconfig-configure";
  runtimeInputs = [ bash ];
  text = ''
    set -euo pipefail

    if [[ "''${1:-}" == "--help" || "''${1:-}" == "-h" ]]; then
      cat <<'EOF'
    jconfig-configure: run jconfig's configure.sh through a Nix app wrapper

    Usage:
      nix run .#configure
      JCONFIG_ROOT=/path/to/jconfig nix run .#configure

    The wrapper resolves JCONFIG_ROOT from $JCONFIG_ROOT or $PWD.
    EOF
      exit 0
    fi

    root="''${JCONFIG_ROOT:-$PWD}"
    if [[ ! -x "$root/configure.sh" ]]; then
      echo "jconfig-configure: run from jconfig root or set JCONFIG_ROOT" >&2
      exit 1
    fi

    exec "$root/configure.sh" "$@"
  '';
}
