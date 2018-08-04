module View.Board exposing (render2D, render3D)

import Data.Board as Board exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)
import Msg exposing (Msg(..))


render3D : Board Cubic -> Html Msg
render3D board =
    List.map move (Board.moves board)
        |> div [ class "board" ]


render2D : Board Flat -> Html Msg
render2D board =
    List.map move (Board.moves board)
        |> div [ class "board" ]


move : Positioned3D -> Html Msg
move m =
    let
        move =
            { player = m.player, column = m.x, row = m.y }

        idx =
            m.z
    in
    button [ class "tile", onClick <| Play move idx ]
        [ text <| toString m.player ]
