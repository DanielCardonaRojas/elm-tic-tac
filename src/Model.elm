module Model exposing (..)

import Data.Board as Board exposing (Board, Cubic, Flat)
import Data.Game as Game exposing (Game)
import Data.Player as Player exposing (Player)


type alias Model =
    { game : Game
    , turn : Player -- The player next to move
    , winner : Maybe Player
    , player : Maybe Player -- The local player, assigned be websocket server.
    , opponent : Maybe Player
    , isReady : Bool
    }


default : Model
default =
    { game = Game.cubic 3
    , turn = Player.PlayerX
    , winner = Nothing
    , player = Nothing
    , opponent = Nothing
    , isReady = False
    }
