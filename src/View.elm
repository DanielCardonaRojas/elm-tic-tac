module View exposing (view)

import Html exposing (..)
import Model exposing (..)
import Msg exposing (Msg(..))


view : Model -> Html Msg
view model =
    text "Elm"
