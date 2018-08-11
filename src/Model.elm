module Model exposing (..)

import Data.Game as Game exposing (Game)
import Data.Player as Player exposing (Player)


type Scene
    = MatchSetup
    | PlayerChoose
    | GamePlay
    | Rematch


type alias Model =
    { game : Game
    , turn : Player -- The player next to move
    , player : Maybe Player -- The local player, assigned be websocket server.
    , opponent : Maybe Player
    , socketId : Maybe String
    , room : Maybe String
    , scene : Scene
    }


default : Model
default =
    { game = Game.make 3
    , turn = Player.PlayerX
    , player = Nothing
    , opponent = Nothing
    , socketId = Nothing
    , room = Nothing
    , scene = MatchSetup
    }
