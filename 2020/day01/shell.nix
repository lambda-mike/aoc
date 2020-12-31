let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell rec {

  name = "AoC2020-Day01-Prolog";

  buildInputs = with pkgs; [
    gprolog
  ];

}
