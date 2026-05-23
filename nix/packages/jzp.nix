{ writeShellApplication, symlinkJoin, coreutils, direnv, nix }:

let
  jzpApp = writeShellApplication {
    name = "jzp";
    runtimeInputs = [ coreutils direnv nix ];
    text = ''
      set -euo pipefail

      case "''${1:-doctor}" in
        doctor|--version|-v)
          echo "jzp: jconfig Nix profile marker"
          echo "root: ${toString ../..}"
          if command -v nix >/dev/null 2>&1; then
            nix --version
          fi
          if command -v direnv >/dev/null 2>&1; then
            direnv version
          fi
          ;;
        help|-h|--help)
          cat <<'EOF'
      jzp: jconfig Nix profile marker

      Usage:
        jzp doctor   Show profile/root information
        jzp help     Show this help

      Install system-wide user profile with:
        nix run /home/jhaag/jconfig#install-jzp-profile

      This package intentionally starts small; stable dev tools can graduate here
      after they are proven in devShells.default.
      EOF
          ;;
        *)
          echo "jzp: unknown command '$1'" >&2
          echo "Try: jzp help" >&2
          exit 2
          ;;
      esac
    '';
  };
in
symlinkJoin {
  name = "jzp";
  paths = [
    jzpApp
    direnv
  ];
}
