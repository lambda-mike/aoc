let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixos;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2020-Day25-Elm";

  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-language-server
    elmPackages.elm-test
  ];

}
