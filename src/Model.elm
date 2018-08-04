module Model exposing (..)

import Data.Board as Board exposing (Board, Cubic, Flat)
import Data.Move as Move exposing (Player)


type Game
    = Simple (Board Flat)
    | Advanced (Board Cubic)


type alias Model =
    { game : Game
    , turn : Player -- The player next to move
    , winner : Maybe Player
    , player : Player -- The local player, assigned be websocket server.
    }


default : Model
default =
    { game = Simple <| Board.flat 3
    , turn = Move.PlayerX
    , winner = Nothing
    , player = Move.PlayerX
    }
