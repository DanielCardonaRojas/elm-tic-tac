module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (Msg(..))
import View.Board as Board


view : Model -> Html Msg
view model =
    renderGame model.game


renderGame : Game -> Html Msg
renderGame game =
    case game of
        Simple board ->
            Board.render2D board

        Advanced board ->
            Board.render3D board
