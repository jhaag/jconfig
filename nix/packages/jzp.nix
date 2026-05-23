{ writeShellApplication, symlinkJoin, coreutils, direnv, herdr, nix }:

let
  jzpApp = writeShellApplication {
    name = "jzp";
    runtimeInputs = [ coreutils direnv herdr nix ];
    text = ''
      set -euo pipefail

      case "''${1:-help}" in
        sync)
          echo "Syncing Nix profile entries..."
          nix profile upgrade --all
          ;;
        --version|-v)
          echo "jzp: jconfig Nix profile marker"
          ;;
        help|-h|--help)
          cat <<'EOF'
      jzp: jconfig Nix profile marker

      Usage:
        jzp sync     Upgrade Nix profile entries
        jzp help     Show this help

      Install system-wide user profile with:
        nix run /home/jhaag/jconfig#install-jzp-profile

      Profile tools:
        direnv
        herdr
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
    herdr
  ];
}
