{ hostpkgs ? import <nixpkgs> {} }:
let
  nixos_pkgs = hostpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "nixos-19.03";
    sha256 = "1ysggrw89p4j6w0sq9i27f2vi7kq9pjbk2f0hbx1ch6lyjh5klab";
  };

  nixos_unstable_pkgs = hostpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "nixos-unstable";
    sha256 = "1lvz85yc7s0yzix3hbcl9m8ahps1jdr9v2sbyq0phfawwjbgswyd";
  };

  pkgs = if hostpkgs.stdenv.isDarwin
          then import nixos_pkgs {}
          else import nixos_pkgs {};
  upkgs = if hostpkgs.stdenv.isDarwin
          then import nixos_unstable_pkgs {}
          else import nixos_unstable_pkgs {};

  nixPackages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.less
    pkgs.git
    upkgs.llvm_8
    pkgs.clang_7
    pkgs.gdb
  ];
in
pkgs.stdenv.mkDerivation {
  name = "ttc";
  buildInputs = nixPackages;
}