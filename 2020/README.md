# Advent of Code 2020

My goal for AoC 2020 is to solve all puzzles using 25 different programming
languages.

## Day 01 - Prolog

Build executable (_build.sh_):

`gplc --no-top-level day01.pl`

Run: `./day01`

Run interpreter and compile code on start:

`gprolog --init-goal '[day01]'`

Run interpreter, compile code and execute main fn:

`gprolog --init-goal '[day01]' --query-goal 'main'"`

Testing (both sample and real data):

```sh
gprolog --init-goal '[day01]'
| ?- mainA('sample.txt', A).
| ?- mainB('input.txt', A).
| ?- main.
```

TODO: replace hack with `catch` to read numbers from file in a better way.

## Day 02 - Idris2

Run main fn:

`nix-shell --run 'idris2 -x main day02.idr'`

Build program:

`./build.sh`

Run program:

`./run.sh`

Interactive repl:

`./repl.sh`

Execute fn interactively:

`:exec main`

Import module:

`:module Data.Strings`

