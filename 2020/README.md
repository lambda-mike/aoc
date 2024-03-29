# Advent of Code 2020

My goal for AoC 2020 is to solve all puzzles using 25 different programming
languages.

All commands are run inside `nix-shell`.

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

`idris2 -x main day02.idr`

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

## Day 03 - Forth

### Run

Run sample: `./samle.sh`

Run main: `./run.sh`

or manually:

```
gforth day03.fs`
samle
main
bye
```

### Test

Test: `./tests.sh` or `gforth day03.fs tests.fs -e bye`

Interactive repl:

```
gforth day03.fs
sample
```

or

```
gforth
s" day03.fs" included \ to load the file manually
sample
```

To exit hit: `Ctrl+D`.

[Space Related applications of Forth](https://web.archive.org/web/20101024223709/http://forth.gsfc.nasa.gov/)

## Day 04 - ATS2

### Build

Run: `./build.sh`

### Run

After building, run: `./day04_dats`

### Experiments

Run: `nix-shell --run 'myatscc experiments.dats' && ./experiments_dats`

## Day 05 - Carp

### Build

`./build.sh`

### Run

`./run.sh`

### Time

After building, run:

`nix-shell --run 'time ./out/Aoc2020-Day05-Carp'`

## Day 06 - Gleam

### Build

`./build.sh`

### Test

`./test.sh`

### Run

`./run.sh`

### Time

After building, run:

`nix-shell --run 'time ./_build/default/bin/aoc_day_six'`

## Day 07 - Eiffel

### Build

`./build.sh`

### Run

`./run.sh`

### Time

After building, run:

`bash -c 'time ./day07'`

## Day 08 - Common Lisp

### Test

Run Lisp REPL:

`sbcl --load day08.lisp`

Inside after changes:

`(load "day08.lisp")`

`(load "tests.lisp")`

### Run

`./run.sh`

### Time

`bash -c 'time sbcl --script day08.lisp'`

## Day 09 - Chapel

### Build

Source Chapel in bash if you installed it locally (or use Docker instead):

`source /path/to/chapel/util/quickstart/setchplenv.bash`

`chpl -o day09 day09.chpl`

Docker:

`docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp chapel/chapel:1.23.0 chpl --static -o day09docker day09.chpl`

### Run

`./day09`

### Time

`bash -c 'time ./day09'`

## Day 10 - Ada

### Build

`./build.sh`

### Run

`./day10`

### Time

`bash -c 'time ./day10'`

## Day 11 - D

### Build

`./build.sh`

### Run

`./day11`

or build & run in one step:

`dmd -run day11.d`

### Time

`bash -c 'time ./day11'`

## Day 12 - Perl

### Run

`./day12.pl`

### Time

`bash -c 'time ./day12.pl'`

## Day 13 - Clojure

`lein repl`

`cider-connect`

`C-c C-k` - Load buffer to Cider

Run `(-main)` in Cider

### Build

`lein uberjar`

### Run

`lein run`

### Time

In `2020/day13` folder:

`nix-shell --run 'java -jar target/uberjar/day13-1.0-standalone.jar'`

## Day 14 - Lua

### Run

`./run.sh`

### Time

`bash -c 'time lua ./day14.lua'`

## Day 15 - Racket

### Build exe

`raco exe --gui day15.rkt`

### Test

`./test.sh`

`raco test day15.rkt`

### Run

`./run.sh`

`racket day15.rkt`

### Time

`bash -c 'time racket ./day15.rkt`

## Day 16 - Unison

### Set up

```
ucm -C .
cd aoc2020.day16
fork .base lib.base
run main
```

### Build

```
compile main day16.uc
```

### Test

Runs automatically on every save inside UCM.

### Time

`bash -c 'time ucm run.compiled day16.uc'`

## Day 17 - Crystal

### Build

`crystal build --progress --release day17.cr`

### Format

`crystal run day17.cr`

### Run

`crystal run day17.cr`

### Time

`bash -c 'time ./run.sh'`

## Day 18 - Kotlin

### Build

`kotlinc day18.kt -include-runtime -d day18.jar`

### Run

`java -jar day18.jar`

### Time

`bash -c 'time java -jar day18.jar'`

## Day 19 - Nim

### Build

`nim c day19.nim`

Optimized version:

`nim c -d:release -x:off --opt:speed day19.nim`

### Run

`nim c -r day19.nim`

### Time

`bash -c './day19'`

## Day 20 - Zig

### Build

`zig build-exe day20.zig`

`zig build-exe -O ReleaseFast day20.zig`

### Run

`zig run day20.zig`

`zig run --watch day20.zig`

### Test

`zig test day20.zig`

### Format

`zig fmt day20.zig`

### Time

`bash -c 'time ./day20'`

## Day 21 - OCaml

How to define String Set:

```
module StrSet = Set.M(String)
type strSet = StrSet.t
```

### Build

`ocamlfind ocamlopt -linkpkg -package base -package stdio day21.ml -o day21`

`dune build day21.exe`

### REPL

`utop`

`#require "base";;`

### Run

`dune exec ./day21.exe`

### Format

`dune fmt`

### Time

`bash -c 'time ./day21'`

## Day 22 - Dart

### Build

`dart compile exe day22.dart`

### Run

`dart run day22.dart`

### Time

`bash -c 'time ./day22.exe'`

## Day 23 - Swift

### Run

`swift day23.swift`

### Time

`bash -c 'time swift day23.swift'`

## Day 24 - Julia

### Run

`julia day24.jl`

### REPL

`julia -i day24.jl`

### Time

`bash -c 'time julia day24.jl`

## Day 25 - elm

### Run

`elm-test`

This will run inside the test and print result.
