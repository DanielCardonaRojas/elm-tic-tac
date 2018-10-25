module View.Setup exposing (connection, playerPicker)

import Constants as Const
import Data.Player as Player exposing (Player(..))
import Element exposing (Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))


connection : Bool -> String -> Element Msg
connection enabled room =
    let
        containerAttrs =
            [ Background.color Const.ui.themeColor.paneBackground
            , Element.spacing 20
            , Element.paddingXY 20 30
            , Font.color Const.colors.gray
            , Border.rounded 10
            ]

        button txt msg =
            Input.button
                [ Element.transparent <| not enabled
                , Element.centerX
                , Element.padding 10
                , width fill
                , Background.color <| Const.ui.themeColor.paneButtonBackground
                ]
                { label = el [ Element.centerX ] <| text txt, onPress = Just msg }
    in
    Element.column containerAttrs
        [ Input.text [ Element.transparent <| not enabled ]
            { onChange = String.trim >> String.toLower >> RoomSetup
            , placeholder = Just <| Input.placeholder [] <| text "Enter room name"
            , label = Input.labelAbove [ Font.size Const.ui.fontSize.medium ] <| text "Create a new match or join one"
            , text = room
            }
        , button "Create Game" <| CreateGame room
        , button "Join" <| SelectRoom room
        , el [ Element.centerX, Font.size Const.ui.fontSize.small ] <| text "Elm-Tic-Tac is a two player online 3D tic tac toe game"
        ]


playerPicker : Model -> Element Msg
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
            Input.button ([ Element.transparent <| not <| enabledFor player ] ++ activeAttr player)
                { label = text <| "Player" ++ Player.toString player, onPress = Just <| SetPlayer player }
    in
    Element.column [ class "picker" ]
        [ el [] <| text "Choose a player"
        , segment Player.PlayerX
        , segment Player.PlayerO
        ]


class =
    Element.htmlAttribute << Html.Attributes.class


playerClass p =
    "player" ++ Player.toString p |> String.toLower |> class
