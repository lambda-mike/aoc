let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

  niv = (import sources.niv {}).niv;

in pkgs.mkShell {

  name = "AdventOfCode2018Python";

  buildInputs = [
    niv
    pkgs.python312
    pkgs.python312Packages.pytest
    pkgs.python312Packages.python-lsp-server
  ];

}
