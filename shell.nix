{ pkgs ? (import <nixpkgs> {}) }:
let
  inherit (pkgs) stdenv;
  inherit (pkgs.localHaskellPackages) purescript;
  inherit (pkgs.nodePackages) bower grunt-cli bower2nix npm2nix;
  inherit (pkgs.gitAndTools) gitFull;

in stdenv.mkDerivation rec {
  name = "ptext-env";
  version = "0.1.0";
  src = ./.;
  buildInputs = [ purescript bower grunt-cli gitFull bower2nix npm2nix ];
}
