let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2020-Day19-Nim";

  buildInputs = with pkgs; [
    nim
    nimlsp
  ];

}
