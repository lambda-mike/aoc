let

  sources = import ./nix/sources.nix;

  nixpkgs = sources."nixpkgs";

  pkgs = import nixpkgs {};

in pkgs.mkShell rec {

  name = "scala-shell";

  buildInputs = with pkgs; [
    niv
    mill
    ammonite
  ];

}
