module Day25Test exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Day25 exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "AoC 2020 - Elm"
        [ test "Day25A" <|
            \_ ->
                let
                    keys =
                        parseInput input

                    result =
                        Debug.log "Solving Day25..." (solve keys)
                in
                -- too low
                Expect.equal result 19414467
        , test "sample" <|
            \_ ->
                let
                    keys =
                        parseInput sample

                    result =
                        Debug.log "Solving Day25 sample..." (solve keys)
                in
                Expect.equal result 14897079
        , describe "parseInput"
            [ test "simple case" <|
                \_ ->
                    let
                        str =
                            "123\n456"

                        expected =
                            { card = 123
                            , door = 456
                            }
                    in
                    Expect.equal expected (parseInput str)
            , test "real case" <|
                \_ ->
                    let
                        str =
                            input

                        expected =
                            { card = 19774466
                            , door = 7290641
                            }
                    in
                    Expect.equal expected (parseInput str)
            ]
        , describe "findLoopSize"
            [ test "17807724" <|
                \_ ->
                    let
                        key =
                            17807724

                        subjectNum =
                            7

                        expected =
                            11
                    in
                    Expect.equal expected (findLoopSize key subjectNum)
            , test "5764801" <|
                \_ ->
                    let
                        key =
                            5764801

                        subjectNum =
                            7

                        expected =
                            8
                    in
                    Expect.equal expected (findLoopSize key subjectNum)
            ]
        ]
