{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  nixConfig.bash-prompt = "\\e[1;32m\[dev\]\\u@\\w$ \\e[m";
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShells.default = import ./shell.nix { inherit pkgs; };
        }
      );
}
