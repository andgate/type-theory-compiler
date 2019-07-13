{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    mtl
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.less
    pkgs.git
    pkgs.llvm_7
    pkgs.clang_7
    pkgs.gdb
    haskellPackages.cabal-install
  ];
in
pkgs.stdenv.mkDerivation {
  name = "ttc";
  buildInputs = nixPackages;
}