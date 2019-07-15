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

  darwin_pkgs18 = hostpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "nixpkgs-18.03-darwin";
    sha256 = "12f7aiwajvxcpi351yh6yg9l30xivn44k6rj5lk558q0gmc5j0p1";
  };

  darwin_pkgs = hostpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "nixpkgs-19.03-darwin";
    sha256 = "01va89pqny7jh9iic3f6dmywpc5m4gf6v106fsgysg50ghkg1gbb";
  };

  darwin_unstable_pkgs = hostpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "nixpkgs-unstable";
    sha256 = "01pfzhghkpsmbr0v1754gjzdlk5fgs6v1vwmplnh87vjjm6xka1n";
  };

  pkgs18 = import darwin_pkgs18 {};

  pkgs = if hostpkgs.stdenv.isDarwin
          then import darwin_pkgs {}
          else import nixos_pkgs {};

  upkgs = if hostpkgs.stdenv.isDarwin
          then import darwin_unstable_pkgs {}
          else import nixos_unstable_pkgs {};

  nixPackages = [
    pkgs.haskellPackages.ghc_8_6_1
    pkgs.haskellPackages.cabal-install
    pkgs.less
    pkgs.git
    pkgs.gcc
    upkgs.llvm_8
    upkgs.clang_7
    pkgs.gdb
  ];
in
pkgs.stdenv.mkDerivation {
  name = "ttc";
  buildInputs = nixPackages;
}