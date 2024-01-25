{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./poi.nix { }
