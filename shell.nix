{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [ pkgs.hpack pkgs.cabal-install (pkgs.haskellPackages.ghcWithPackages (p: [p.aeson p.bytestring p.containers p.directory p.hspec p.path p.polysemy p.polysemy-plugin p.polysemy-zoo p.split])) ];
}
