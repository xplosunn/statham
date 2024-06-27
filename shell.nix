{ pkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/54b701f3f82b3a1ec42dfa9b9b9ba9994d3ca5b8.tar.gz")
  { } }:
let
  jre = pkgs.jre;
  sbt = pkgs.sbt.override { jre = jre; };
in
pkgs.mkShell {
  buildInputs = [
    jre
    sbt
  ];
}
