{ writeShellApplication, nix }:

writeShellApplication {
  name = "jconfig-install-jzp-profile";
  runtimeInputs = [ nix ];
  text = ''
    set -euo pipefail

    root="''${JCONFIG_ROOT:-$PWD}"
    if [[ ! -f "$root/flake.nix" ]]; then
      echo "jconfig-install-jzp-profile: run from jconfig root or set JCONFIG_ROOT" >&2
      exit 1
    fi

    flake_ref="path:$root#jzp"

    if nix profile list | grep -Fq "$flake_ref"; then
      echo "jzp profile already installed from $flake_ref"
      echo "To update it after changing jconfig, run: nix profile upgrade '.*jzp.*'"
      exit 0
    fi

    echo "Installing jzp profile from $flake_ref"
    nix profile install "$flake_ref"
  '';
}
