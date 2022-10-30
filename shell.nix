{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  packages = [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.ormolu
  ];
  shellHooks = ''
    alias b="cabal build"
    alias c="cabal clean"
    alias fmt="ormolu -i ./**/*.hs"
    alias r="cabal run --"
    alias repl="cabal repl"
    alias t="cabal test"
  '';
}
