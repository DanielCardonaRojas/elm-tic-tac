module Msg exposing (..)

import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)


type Msg
    = Play Move Int -- Local player move
    | Opponent Move Int -- Remote player move
    | NewGameMulti Int
    | SetPlayer Player
    | SetOponent Player
    | PlayAgainMulti Int
    | NoOp
