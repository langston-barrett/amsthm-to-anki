# To use: nix-shell --run zsh
# https://nixos.org/wiki/Development_Environments
# http://nixos.org/nix/manual/#sec-nix-shell
let
  pkgs = import <nixpkgs> {};
in with pkgs; stdenv.mkDerivation {
  name = "amsthm-to-anki";
  src = null;
  buildInputs = [
    gcc                   # required to build GHC
    git                   # stack needs this to fetch from github
    gmp                   # required to build GHC
    haskellPackages.stack # stack will handle the haskell packages
  ];
}
