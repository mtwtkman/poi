{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  packages = [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.ormolu
  ];
  shellHook = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias fmt="ormolu -i ./**/*.hs"
    alias r="cabal run --"
    alias repl="cabal repl"
    alias t="cabal test"
    alias devrun="POI_ROOT=sandbox r poi"
  '';
}
