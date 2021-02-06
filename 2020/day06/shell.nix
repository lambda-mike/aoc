let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell rec {

  name = "AoC2020-Day06-Gleam";

  buildInputs = with pkgs; [
    erlang
    gleam
    rebar3
  ];

}
