{
  description = "jconfig reproducible tool and shell entrypoints";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    herdr = {
      url = "github:ogulcancelik/herdr/v0.6.2";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, herdr }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system:
          f {
            inherit system;
            pkgs = import nixpkgs { inherit system; };
          });
    in
    {
      packages = forAllSystems ({ pkgs, system }:
        let
          herdrPackage = herdr.packages.${system}.herdr;
          jzp = pkgs.callPackage ./nix/packages/jzp.nix {
            herdr = herdrPackage;
          };
        in
        {
          inherit jzp;
          herdr = herdrPackage;
          default = jzp;
        });

      devShells = forAllSystems ({ pkgs, ... }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            bashInteractive
            git
            jq
            nodejs_24
            python313
            ripgrep
            shellcheck
            uv
          ];

          shellHook = ''
            echo "jconfig nix dev shell"
            echo "  node:   $(node --version 2>/dev/null || true)"
            echo "  npm:    $(npm --version 2>/dev/null || true)"
            echo "  python: $(python --version 2>/dev/null || true)"
            echo "  uv:     $(uv --version 2>/dev/null || true)"
          '';
        };
      });

      apps = forAllSystems ({ pkgs, system }:
        let
          configure = pkgs.callPackage ./nix/apps/configure.nix { };
          herdrPackage = herdr.packages.${system}.herdr;
          installProfile = pkgs.callPackage ./nix/apps/install-profile.nix { };
          sync = pkgs.callPackage ./nix/apps/sync.nix { };
        in
        {
          configure = {
            type = "app";
            program = "${configure}/bin/jconfig-configure";
          };

          install-jzp-profile = {
            type = "app";
            program = "${installProfile}/bin/jconfig-install-jzp-profile";
          };

          sync = {
            type = "app";
            program = "${sync}/bin/jconfig-sync";
          };

          jzp = {
            type = "app";
            program = "${self.packages.${system}.jzp}/bin/jzp";
          };

          herdr = {
            type = "app";
            program = "${herdrPackage}/bin/herdr";
          };

          default = self.apps.${system}.jzp;
        });

      formatter = forAllSystems ({ pkgs, ... }: pkgs.nixfmt);
    };
}
