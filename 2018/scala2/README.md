# Advent of Code 2018 Scala Edition

## Build

`mill day01.compile`

`mill day01.run`

## Test

`mill -w day01.test`

`mill all _.test`

## Run & Measure

`mill day01.assembly`

This defaults to Java14:

`nix-shell -p jdk --run 'time ./out/day01/assembly/dest/out.jar'`

