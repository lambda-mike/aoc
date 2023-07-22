#!/usr/bin/env bash
nix-shell --run 'nim c -d:release -x:off --opt:speed day19.nim'
