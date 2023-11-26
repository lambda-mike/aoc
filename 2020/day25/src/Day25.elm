module Day25 exposing (findLoopSize, input, parseInput, sample, solve)

import String exposing (toInt)


type alias Key =
    Int


type alias PublicKeys =
    { card : Key
    , door : Key
    }


type alias LoopSize =
    Int


input : String
input =
    "19774466\n7290641"


sample : String
sample =
    "17807724\n5764801"


parseInput : String -> PublicKeys
parseInput str =
    let
        rows =
            str
                |> String.trim
                |> String.split "\n"

        parseInt n =
            Maybe.withDefault 0 (toInt n)
    in
    case rows of
        card :: door :: [] ->
            { card = parseInt card
            , door = parseInt door
            }

        -- unsupported case
        _ ->
            { card = 0, door = 0 }


publicSubjectNumber : Int
publicSubjectNumber =
    7


initialTransformationValue : Int
initialTransformationValue =
    1


{-| returns new value given subject number and value
-}
transform : Int -> Int -> Int
transform subjectNum value =
    let
        transformationConstant =
            20201227
    in
    value
        * subjectNum
        |> modBy transformationConstant


findLoopSize : Key -> Int -> LoopSize
findLoopSize key subjectNum =
    let
        go i val =
            if val == key then
                i

            else
                go (i + 1) (transform subjectNum val)
    in
    go 0 initialTransformationValue


generateEncryptionKey : LoopSize -> Key -> Key
generateEncryptionKey loopSize subjectNum =
    let
        go i val =
            if i == loopSize then
                val

            else
                go (i + 1) (transform subjectNum val)
    in
    go 0 initialTransformationValue


solve : PublicKeys -> Key
solve keys =
    let
        doorLoopSize =
            findLoopSize keys.door publicSubjectNumber

        encryptionKey =
            generateEncryptionKey doorLoopSize keys.card
    in
    encryptionKey
