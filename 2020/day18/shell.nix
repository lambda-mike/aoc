let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2020-Day18-Kotlin";

  buildInputs = with pkgs; [
    kotlin
    kotlin-language-server
    ktlint
  ];

}
