let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2020-Day17-Crystal";

  buildInputs = with pkgs; [
    crystal
    crystalline
    gcc
    pcre
  ];

}
