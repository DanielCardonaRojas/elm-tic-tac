module Data.Taco exposing (Taco)

import Data.Game as Game exposing (Game)
import Data.Player as Player exposing (Player)


type alias Taco =
    { game : Game
    , turn : Player -- The player next to move
    , player : Maybe Player -- The local player, assigned be websocket server.
    , opponent : Maybe Player
    , socketId : Maybe String
    , room : Maybe String
    }
