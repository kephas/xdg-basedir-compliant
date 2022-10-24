{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [ (pkgs.haskellPackages.ghcWithPackages (p: [p.aeson p.bytestring p.hspec p.polysemy p.polysemy-plugin p.QuickCheck p.split])) ];
}
