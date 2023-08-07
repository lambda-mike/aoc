#!/usr/bin/env bash
nix-shell --run 'zig build-exe -O ReleaseFast day20.zig'
