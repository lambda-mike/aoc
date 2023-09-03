let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixos;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2020-Day23-Swift";

  buildInputs = with pkgs; [
    swift
    swift-format
  ];

}
