module View exposing (view)

--import Html.Attributes exposing (..)

import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import View.Game as Game

view : Model -> Html Msg
view model =
    case model.scene of 
        MatchSetup str ->
            gameSetup (model.room == Nothing) str
            |> template
        Rematch ->
            rematch model
            |> template
        GamePlay ->
            model.player
            |> Maybe.map (Game.render model.game) 
            |> Maybe.withDefault (text "No player in game")
            |> template
        PlayerChoose ->
            playerPicker model
            |> template
        
template : Html Msg -> Html Msg
template html =
    div [ class "elm-tic-tac" ]
        [ span [ class "gametitle" ] [ text "Elm-Tic-Tac" ]
        , html
        , footer
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


footer : Html msg
footer =
    div [ class "footer" ]
        [ span []
            [ text "The code for this game is open sourced and written in Elm" ]
        , span
            []
            [ text "© 2018 Daniel Cardona Rojas" ]
        ]

-- Scenes
gameSetup : Bool -> String  -> Html Msg
gameSetup enabled room =
    div [class "setup"]
    [ h3 [] [text "Create a new match or join one"]
    , input [ onInput (String.trim >> String.toLower >> RoomSetup), placeholder "Enter room name", disabled <| not enabled ] []
    , button [onClick <| CreateGame room, disabled <| not enabled ] [text "Create Game"]
    , button [onClick <| SelectRoom room, disabled <| not enabled ] [text "Join"]
    ]

rematch : Model -> Html Msg
rematch model =
    div [class "rematch"] [text "Rematch"]


playerPicker : Model -> Html Msg
playerPicker model =
    let
        activeAttr player =
            if Maybe.map (\p -> p == player) model.player |> Maybe.withDefault False then
                [ class "is-active" ]
            else
                []

        playerClass p =
            case p of
                Player.PlayerX ->
                    class "playerx"

                Player.PlayerO ->
                    class "playero"

        enabledFor player =
            Maybe.map (\p -> p /= player) model.opponent
                |> Maybe.withDefault True

        segment player =
            button
                (playerClass player :: (onClick <| SetPlayer player) :: (disabled <| not <| enabledFor player) :: activeAttr player)
                [ text <| toString player ]
    in
    div [ class "picker" ]
        [ span [] [ text "Choose a player" ]
        , segment Player.PlayerX
        , segment Player.PlayerO
        ]
