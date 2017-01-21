# To use: run `nix-shell` or `nix-shell --run "exec zsh"`
# https://nixos.org/wiki/Development_Environments
# http://nixos.org/nix/manual/#sec-nix-shell

let
  _nixpkgs = import <nixpkgs> { };
  pkgs = import (_nixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    # 16.09 fails to build, so this is a recent (2017-01) master
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
