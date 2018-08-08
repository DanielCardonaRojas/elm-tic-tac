module Data.Player exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Player
    = PlayerX
    | PlayerO


switch : Player -> Player
switch p =
    case p of
        PlayerX ->
            PlayerO

        PlayerO ->
            PlayerX


encode : Player -> Value
encode =
    Encode.string << toString


decode : Decoder Player
decode =
    Decode.string
        |> Decode.andThen
            (\str ->
                fromString str
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Error parsing player")
            )


fromString : String -> Maybe Player
fromString str =
    case String.toLower str of
        "x" ->
            Just PlayerX

        "o" ->
            Just PlayerO

        _ ->
            Nothing


toString : Player -> String
toString player =
    case player of
        PlayerX ->
            "X"

        PlayerO ->
            "O"
