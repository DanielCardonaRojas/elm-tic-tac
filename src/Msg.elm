module Msg exposing (..)

import Data.Move as Move exposing (Move)


type Mode
    = SingleBoard Int
    | MultiBoard Int


type Msg
    = Switch Mode
    | Play Move Int
    | Opponent Move Int
    | NoOp
