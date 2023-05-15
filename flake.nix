{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      overlays = [
        haskellNix.overlay
        (final: prev: {
          hixProject = final.haskell-nix.hix.project {
            src = ./.;
            evalSystem = "x86_64-linux";
          };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.hixProject.flake {};
    in
      flake
      // {
        legacyPackages = pkgs;
        devShells.default = flake.devShells.default.overrideAttrs (oldAttrs: {
            LD_PRELOAD = "${pkgs.freeglut}/lib/libglut.so";
          });
        packages.default = flake.packages."reticule-minor:exe:reticule-minor".overrideAttrs (oldAttrs: {
          nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [pkgs.makeWrapper];
          buildInputs = (oldAttrs.buildInputs or []) ++ [pkgs.freeglut];
          postInstall =
            (oldAttrs.postInstall or "")
            + ''
              wrapProgram $out/bin/reticule-minor --set LD_PRELOAD "${pkgs.freeglut}/lib/libglut.so"
            '';
        });
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
