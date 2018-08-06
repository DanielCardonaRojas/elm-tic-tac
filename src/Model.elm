module Model exposing (..)

import Data.Board as Board exposing (Board, Cubic, Flat)
import Data.Player as Player exposing (Player)


type Game
    = Simple (Board Flat)
    | Advanced (Board Cubic)


type alias Model =
    { game : Game
    , turn : Player -- The player next to move
    , winner : Maybe Player
    , player : Maybe Player -- The local player, assigned be websocket server.
    }


default : Model
default =
    --{ game = Simple <| Board.flat 3
    { game = Advanced <| Board.lock <| Board.cubic 3
    , turn = Player.PlayerX
    , winner = Nothing
    , player = Nothing
    }
