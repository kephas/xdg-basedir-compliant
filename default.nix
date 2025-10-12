{ pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz) {} }:

let nix2505 = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz) {};
in

pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = (pkgs.haskell.lib.compose.addBuildTools (with pkgs; [ nix2505.cabal-install hpack haskell-language-server ghcid hlint just ]));
}
