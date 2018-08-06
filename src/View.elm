module View exposing (view)

--import Html.Attributes exposing (..)

import Data.Player as Player
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Msg exposing (Msg(..))
import View.Board as Board


view : Model -> Html Msg
view model =
    div [ class "elm-tic-tac" ]
        [ renderGame model
        , playerPicker model
        ]


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
                    Play { column = pos.column, row = pos.row, player = model.turn } pos.board
                )
                board


playerPicker : Model -> Html Msg
playerPicker model =
    let
        segment player =
            button
                ((onClick <| SetPlayer player)
                    :: (if Maybe.map (\p -> p == player) model.player |> Maybe.withDefault False then
                            [ class "is-active" ]
                        else
                            []
                       )
                )
                [ text <| toString player ]
    in
    div [ class "picker" ]
        [ segment Player.PlayerX
        , segment Player.PlayerO
        ]
