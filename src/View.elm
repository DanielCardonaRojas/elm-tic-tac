module View exposing (view)

--import Html.Attributes exposing (..)

import Html exposing (..)
import Model exposing (..)
import Msg exposing (Msg(..))
import View.Board as Board


view : Model -> Html Msg
view model =
    renderGame model


renderGame : Model -> Html Msg
renderGame model =
    case model.game of
        Simple board ->
            Board.render2D
                (\pos ->
                    Play { column = pos.column, row = pos.row, player = model.turn } 0
                )
                board

        Advanced board ->
            Board.render3D
                (\pos ->
                    Play { column = pos.x, row = pos.y, player = model.turn } pos.z
                )
                board
