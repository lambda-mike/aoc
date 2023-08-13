let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

in pkgs.mkShell {

  name = "AoC2020-Day21-OCaml";

  buildInputs = with pkgs; [
    dune_3
    ocaml
    ocamlPackages.ocamlformat
    ocamlPackages.base
    ocamlPackages.ocaml-lsp
    ocamlPackages.stdio
    ocamlPackages.utop
  ];

}
