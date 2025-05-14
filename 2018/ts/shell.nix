let

  sources = import ./nix/sources.nix;

  nixpkgs = sources.nixpkgs;

  pkgs = import nixpkgs {};

  niv = (import sources.niv {}).niv;

in pkgs.mkShell {

  name = "pnpm-template";

  buildInputs = [
    niv
    pkgs.nodePackages.typescript-language-server
    pkgs.nodejs_20
    pkgs.nodePackages.pnpm
  ];

}
