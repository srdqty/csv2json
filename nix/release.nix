{ compiler ? import ./ghc.nix }:

let
  haskellOverrides = pkgs: new: old: {
    csv2json = old.callPackage ./.. { };
  };

  overlays = import ./overlays.nix {
    compiler = compiler;
    extraHaskellOverride = haskellOverrides;
  };

  pkgs = import ./nixpkgs-pinned {
    overlays = overlays;
  };
in
  pkgs.haskellPackages.csv2json
