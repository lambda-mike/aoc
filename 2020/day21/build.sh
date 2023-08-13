#!/usr/bin/env bash
# nix-shell --run 'ocamlfind ocamlopt -o day21 -linkpkg -package base,stdio day21.ml'
nix-shell --run 'dune build day21.exe'
