module Utils exposing (..)

import Maybe.Extra as Maybe
import Model exposing (..)


shouldStartGame : Model -> Bool
shouldStartGame model =
    Just (\p1 p2 -> p1 /= p2)
        |> Maybe.andMap model.player
        |> Maybe.andMap model.opponent
        |> Maybe.withDefault False
