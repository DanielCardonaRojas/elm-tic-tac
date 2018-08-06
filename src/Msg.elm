module Msg exposing (..)

import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)


type Mode
    = SingleBoard Int
    | MultiBoard Int


type Msg
    = Switch Mode
    | Play Move Int -- Local player move
    | Opponent Move Int -- Remote player move
    | SetPlayer Player
    | NoOp
