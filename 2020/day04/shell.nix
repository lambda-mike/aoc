let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell rec {

  name = "AoC2020-Day04-ATS2";

  buildInputs = with pkgs; [
    ats2
  ];

}
