module Data.Move exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)


type alias Positioned a =
    { a | column : Int, row : Int }


type alias Move =
    Positioned { player : Player }


type Player
    = PlayerX
    | PlayerO


decode : Decoder Move
decode =
    let
        playerDecoder =
            Decode.string
                |> Decode.andThen
                    (\str ->
                        playerFromString str
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail "Error parsing player")
                    )
    in
    Pipeline.decode (\p c r -> { player = p, column = c, row = r })
        |> required "player" playerDecoder
        |> required "column" Decode.int
        |> required "row" Decode.int


encode : Move -> Value
encode move =
    Encode.object
        [ ( "player", Encode.string <| playerToString move.player )
        , ( "column", Encode.int move.column )
        , ( "row", Encode.int move.row )
        ]


playerFromString : String -> Maybe Player
playerFromString str =
    case String.toLower str of
        "x" ->
            Just PlayerX

        "o" ->
            Just PlayerO

        _ ->
            Nothing


playerToString : Player -> String
playerToString player =
    case player of
        PlayerX ->
            "x"

        PlayerO ->
            "o"


equallyPositioned : Positioned a -> Positioned b -> Bool
equallyPositioned pos1 pos2 =
    pos1.row == pos2.row && pos1.column == pos2.column


positioned : Positioned a -> Positioned {}
positioned pos =
    { column = pos.column, row = pos.row }
