#!/usr/bin/env bash
nix-shell --run 'crystal build --progress --release day17.cr'
nix-shell --run 'crystal build --progress --release day17b.cr'
