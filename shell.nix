{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  packages = [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.ormolu
    haskellPackages.cabal2nix
  ];
  shellHook = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias fmt="ormolu -i ./**/*.hs"
    alias r="cabal run --"
    alias repl="cabal repl"
    alias t="cabal test"
    alias release="cabal2nix . > poi.nix && nix-build"
    alias devrun="POI_ROOT=sandbox r poi"
  '';
}
