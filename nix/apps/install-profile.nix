{ writeShellApplication, gnugrep, nix }:

writeShellApplication {
  name = "jaspah-install-jzp-profile";
  runtimeInputs = [ gnugrep nix ];
  text = ''
    set -euo pipefail

    root="''${JASPAH_ROOT:-$PWD}"
    if [[ ! -f "$root/flake.nix" ]]; then
      echo "jaspah-install-jzp-profile: run from jaspah root or set JASPAH_ROOT" >&2
      exit 1
    fi

    flake_ref="path:$root#jzp"

    if nix profile list | grep -Fq "$flake_ref"; then
      echo "jzp profile already installed from $flake_ref"
      echo "To sync Nix profile entries, run: jzp sync"
      exit 0
    fi

    echo "Installing jzp profile from $flake_ref"
    nix profile add "$flake_ref"
  '';
}
