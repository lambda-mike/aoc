let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell rec {

  name = "AoC2020-Day12-Perl";

  buildInputs = with pkgs; [
    perl
  ];

  shellHook = ''
    export LC_ALL=C
  '';

}
