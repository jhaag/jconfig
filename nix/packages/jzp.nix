{ writeShellApplication, symlinkJoin, lib, stdenv, coreutils, direnv, herdr, nix, tmux, xclip }:

let
  jzpApp = writeShellApplication {
    name = "jzp";
    runtimeInputs = [ coreutils direnv herdr nix tmux ];
    text = ''
      set -euo pipefail

      case "''${1:-help}" in
        sync)
          echo "Syncing Nix profile entries..."
          nix profile upgrade --all
          ;;
        --version|-v)
          echo "jzp: jaspah Nix profile marker"
          ;;
        help|-h|--help)
          cat <<'EOF'
      jzp: jaspah Nix profile marker

      Usage:
        jzp sync     Upgrade Nix profile entries
        jzp help     Show this help

      Install system-wide user profile with:
        nix run /home/jhaag/jaspah#install-jzp-profile

      Profile tools:
        direnv
        herdr
        tmux
        xclip (Linux)
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
    tmux
  ] ++ lib.optional stdenv.isLinux xclip;
}
