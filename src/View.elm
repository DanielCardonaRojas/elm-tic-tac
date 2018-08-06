module View exposing (view)

--import Html.Attributes exposing (..)

import Data.Player as Player
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Msg exposing (Msg(..))
import Utils
import View.Board as Board


view : Model -> Html Msg
view model =
    let
        joinText =
            if model.player /= Nothing then
                text "Waiting for opponent to join"
            else
                text "Choose a player"

        mainView =
            if model.isReady then
                renderGame model
            else
                joinText

        playerStatus =
            if Utils.shouldStartGame model then
                text <| "You are " ++ toString model.player
            else
                playerPicker model
    in
    div [ class "elm-tic-tac" ]
        [ mainView
        , playerStatus
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
        activeAttr player =
            if Maybe.map (\p -> p == player) model.player |> Maybe.withDefault False then
                [ class "is-active" ]
            else
                []

        enabledFor player =
            Maybe.map (\p -> p /= player) model.opponent
                |> Maybe.withDefault True

        segment player =
            button
                ((onClick <| SetPlayer player) :: (disabled <| not <| enabledFor player) :: activeAttr player)
                [ text <| toString player ]
    in
    div [ class "picker" ]
        [ segment Player.PlayerX
        , segment Player.PlayerO
        ]
