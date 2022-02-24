{
  description = "w";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.11";

    crate2nix = {
      url = "github:kolloch/crate2nix";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { crate2nix, flake-utils, nixpkgs, rust-overlay, self, ... }:
    let
      name = "w";
      rustChannel = "stable";
      rustVersion = "latest";
      inherit (builtins) attrValues listToAttrs map;
    in flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (import rust-overlay)
          (after: before:
            (listToAttrs (map (element: {
              name = element;
              value = before.rust-bin.${rustChannel}.${rustVersion}.default.override {
                extensions = [ "rust-src" ];
                targets = [ "wasm32-unknown-unknown" ];
              };
            }) [ "cargo" "rustc" ])))
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        buildInputs = with pkgs; [
          cargo
          cargo-tarpaulin
          cargo-watch
        ];
        nativeBuildInputs = [ ];
        inherit (import "${crate2nix}/tools.nix" { inherit pkgs; })
          generatedCargoNix;
        cargoNix = import (generatedCargoNix {
          inherit name;
          src = ./.;
        }) {
          inherit pkgs;
          defaultCrateOverrides = pkgs.defaultCrateOverrides // {
            ${name} = attrs: { inherit buildInputs nativeBuildInputs; };
          };
        };
      in {
        packages.${name} = cargoNix.rootCrate.build;
        # `nix build`
        defaultPackage = self.packages.${system}.${name};
        # `nix flake check`
        checks.${name} = cargoNix.rootCrate.build.override { runTests = true; };
        # `nix run`
        apps.${name} = flake-utils.lib.mkApp {
          inherit name;
          drv = self.packages.${system}.${name};
        };
        defaultApp = self.apps.${system}.${name};
        # `nix develop`
        devShell = pkgs.mkShell {
          #inputsFrom = attrValues self.packages.${system};
          inherit buildInputs nativeBuildInputs;

          shellHook = ''
            export CARGO_INSTALL_ROOT=$PWD/.cargo
            export PATH=$PATH:$CARGO_INSTALL_ROOT/bin
          '';
        };
      });
}
