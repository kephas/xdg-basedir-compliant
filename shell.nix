{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [ (pkgs.haskellPackages.ghcWithPackages (p: [p.hspec p.polysemy p.polysemy-plugin p.QuickCheck])) ];
}
