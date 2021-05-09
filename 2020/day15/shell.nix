let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell rec {

  name = "AoC2020-Day15-Racket";

  buildInputs = with pkgs; [
    racket
  ];

}
