let
  sources = import ./npins;
  pkgs = import sources.nixpkgs {};
in pkgs.mkShell {

  name = "AdventOfCode2018Perl";

  buildInputs = with pkgs; [
    npins
    perl
  ];

  shellHook = ''
    # export LC_ALL=C
  '';

}
