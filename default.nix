{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc948" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./poi.nix { }
