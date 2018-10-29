module Data.Move exposing (..)

import Data.Player as Player exposing (Player)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)


type alias Positioned a =
    { a
        | column : Int
        , row : Int
    }


type alias Positioned3D r =
    { r
        | column : Int
        , row : Int
        , board : Int
    }


type alias Move =
    Positioned { player : Player }


type alias Move3D =
    Positioned3D { player : Player }


decode : Decoder Move
decode =
    Decode.succeed (\p c r -> { player = p, column = c, row = r })
        |> required "player" Player.decode
        |> required "column" Decode.int
        |> required "row" Decode.int


encode : Move -> Value
encode move =
    Encode.object
        [ ( "player", Player.encode move.player )
        , ( "column", Encode.int move.column )
        , ( "row", Encode.int move.row )
        ]


decode3D : Decoder Move3D
decode3D =
    Decode.succeed (\p c r idx -> { player = p, column = c, row = r, board = idx })
        |> required "player" Player.decode
        |> required "column" Decode.int
        |> required "row" Decode.int
        |> required "board" Decode.int


encode3D : Move3D -> Value
encode3D move =
    Encode.object
        [ ( "player", Player.encode move.player )
        , ( "column", Encode.int move.column )
        , ( "row", Encode.int move.row )
        , ( "board", Encode.int move.board )
        ]


equallyPositioned : Positioned a -> Positioned b -> Bool
equallyPositioned pos1 pos2 =
    pos1.row == pos2.row && pos1.column == pos2.column


positioned : Positioned a -> Positioned {}
positioned pos =
    { column = pos.column, row = pos.row }


positioned3D : Positioned3D a -> Positioned3D {}
positioned3D pos =
    { column = pos.column
    , row = pos.row
    , board = pos.board
    }


from2D : Int -> Positioned { player : m } -> Positioned3D { player : m }
from2D n pos2D =
    { column = pos2D.column
    , row = pos2D.row
    , board = n
    , player = pos2D.player
    }


as2D : Positioned3D { player : m } -> Positioned { player : m }
as2D pos =
    { player = pos.player
    , column = pos.column
    , row = pos.row
    }


fromMoveInBoard : Int -> Move -> Move3D
fromMoveInBoard idx move =
    { player = move.player
    , column = move.column
    , row = move.row
    , board = idx
    }
