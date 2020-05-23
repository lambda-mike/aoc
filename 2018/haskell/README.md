# aoc2018-haskell

## Test

`stack test`

`stack test --file-watch`

`stack test --profile`

First option allows running specific parts of spec

`stack test --ta '-m "Day19A"'`

`stack test --ta --match="Day19A"`

`stack test --ta --match="/Day9A/solve/10"`

## Develop

`ghcid`

`stack ghci`

## Build

`stack build`

## Execute

`stack exec aoc2018-day19a`

## Measure time

```
bash
time $(stack exec "aoc2018-day09")
```

## Profiling

build:
  library-profiling: true
  executable-profiling: true

`$ stack exec aoc2018-day19a -- +RTS -p -RTS`

Other options:

- `-h`

- `-hy`

`hp2ps -c result.hp`
