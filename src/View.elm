module View exposing (view)

--import Html.Attributes exposing (..)

import Data.Game as Game exposing (Game, Mode(..), Status(..))
import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import Utils
import View.Board as Board


view : Model -> Html Msg
view model =
    let
        mainView =
            if model.isReady then
                renderGame model
            else
                div [ class "createsession" ] []

        playerStatus =
            if Utils.shouldStartGame model then
                text <| "You are player " ++ (Maybe.map Player.toString model.player |> Maybe.withDefault "")
            else
                playerPicker model
    in
    div [ class "elm-tic-tac" ]
        [ span [ class "gametitle" ] [ text "Elm-Tic-Tac" ]
        , mainView
        , playerStatus
        ]


renderGame : Model -> Html Msg
renderGame model =
    let
        winTitle winner =
            Maybe.map
                (\p ->
                    if p == winner then
                        "You win"
                    else
                        "You loose"
                )
                model.player
                |> Maybe.withDefault "Tie"

        rematchButton winner =
            Maybe.map
                (\p ->
                    if p == winner then
                        text "Waiting for rematch request"
                    else
                        button [ onClick <| PlayAgainMulti 3 ] [ text "Rematch" ]
                )
                model.player
                |> Maybe.withDefault (text "Tie")

        lockedBoard =
            Maybe.map (renderGameMode <| Game.lock model.game) model.player
                |> Maybe.withDefault (playerPicker model)

        board =
            Maybe.map (renderGameMode model.game) model.player
                |> Maybe.withDefault (playerPicker model)
    in
    case Game.status model.game of
        Winner p moves ->
            div [ class "game" ]
                [ text <| winTitle p
                , rematchButton p
                , text <| toString moves
                , lockedBoard
                ]

        Tie ->
            div [ class "game" ]
                [ text "Tie"
                , button [ onClick <| PlayAgainMulti 3 ] [ text "Play Again" ]
                , lockedBoard
                ]

        Playing ->
            div [ class "game" ]
                [ turnIndicator model
                , board
                ]


renderGameMode : Game -> Player -> Html Msg
renderGameMode game nextPlayer =
    case game.mode of
        Simple board ->
            Board.render2D
                (\pos ->
                    Play { column = pos.column, row = pos.row, player = nextPlayer } 0
                )
                board

        Advanced board ->
            Board.render3D
                (\pos ->
                    Play { column = pos.column, row = pos.row, player = nextPlayer } pos.board
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
        [ span [] [ text "Choose a player" ]
        , segment Player.PlayerX
        , segment Player.PlayerO
        ]


turnIndicator : Model -> Html Msg
turnIndicator model =
    let
        turnMessage player turn =
            if player == turn then
                span [ class "active" ]
                    [ text "Your turn" ]
            else
                span [ class "inactive" ]
                    [ text "Waiting for opponent move" ]
    in
    Just turnMessage
        |> Maybe.andMap model.player
        |> Maybe.andMap (Just model.turn)
        |> Maybe.withDefault (text "Board blocked")
