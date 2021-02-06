#!/usr/bin/env bash
nix-shell --run 'rebar3 compile && rebar3 escriptize'
