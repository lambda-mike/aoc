let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2023-Julia";

  buildInputs = with pkgs; [
    julia
  ];

  JULIA_PROJECT = "@.";

}
