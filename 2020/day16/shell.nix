let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2020-Day16-Unison";

  buildInputs = with pkgs; [
    unison-ucm
  ];

}
