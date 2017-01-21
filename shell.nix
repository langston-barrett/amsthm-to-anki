# To use: nix-shell --run zsh
# https://nixos.org/wiki/Development_Environments
# http://nixos.org/nix/manual/#sec-nix-shell

let
  _nixpkgs = import <nixpkgs> { };
  # 16.09 from https://github.com/NixOS/nixpkgs/releases
  pkgs = import (_nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    #rev   = "16.09"; # build fails mysteriously...
    rev   = "81d9893bcd9d96b4c7aabea4381f6765289ef722";
    sha256 = "0phjhlxlb4cwyrpj9gpz3caf13qbawijr7rq46a8id5ak92kawah";
  }) {};
in with pkgs; stdenv.mkDerivation {
  name = "amsthm-to-anki";
  src = null;
  buildInputs = [
    gcc                      # required to build GHC
    git                      # stack needs this to fetch from github
    gmp                      # required to build GHC
    haskellPackages.hindent  # re-indent source code
    haskellPackages.stack    # stack will handle the haskell packages
  ];
}
