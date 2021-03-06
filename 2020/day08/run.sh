#!/usr/bin/env bash
#sbcl --load day08.lisp --non-interactive --eval "(main)" --eval "(exit)"
nix-shell --run 'sbcl --script day08.lisp '
