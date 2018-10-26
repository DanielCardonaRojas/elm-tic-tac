module View exposing (view)

--import Html exposing (Html)

import Constants as Const
import Data.Player as Player exposing (Player)
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import View.Game as Game
import View.Setup as Setup


view model =
    Element.layout [] (viewElement model)


viewElement : Model -> Element Msg
viewElement model =
    case model.scene of
        MatchSetup str ->
            Element.column [ Element.centerX ]
                [ Setup.connection (model.room == Nothing) str
                , maybe (roomInfo []) model.room
                ]
                |> template

        PlayerChoose ->
            Element.column [ Element.centerX ]
                [ Setup.playerPicker model
                , maybe (roomInfo []) model.room
                ]
                |> template

        GamePlay ->
            Element.row [ class "gameplay", Element.centerX, width fill, height fill ]
                [ leftPortion model
                , centerPortion model
                , rightPortion model
                ]
                |> template

        Rematch ->
            rematch model
                |> template


centerPortion : Model -> Element Msg
centerPortion model =
    maybe
        (Game.render
            [ class "game_"
            , width <| Element.fillPortion 2
            , height fill
            , Element.padding Const.ui.spacing.normal
            ]
            model.game
        )
        model.player


leftPortion : Model -> Element Msg
leftPortion model =
    Element.column
        [ width <| Element.fillPortion 1
        , height fill
        , Element.padding Const.ui.spacing.normal
        ]
        [ maybe (\p -> playerScore (Tuple.first model.score) (model.turn /= p) "You" p) model.player ]


rightPortion : Model -> Element Msg
rightPortion model =
    Element.column
        [ width <| Element.fillPortion 1
        , height fill
        , Element.padding Const.ui.spacing.normal
        ]
        [ maybe (\p -> playerScore (Tuple.second model.score) (model.turn /= p) "Opponent" p) model.opponent
        , maybe (roomInfo [ Element.alignBottom ]) model.room
        ]


maybe f =
    Maybe.map f >> Maybe.withDefault Element.none


roomInfo : List (Attribute Msg) -> String -> Element Msg
roomInfo attrs roomName =
    el attrs <|
        text <|
            "Connected to room: "
                ++ roomName


template : Element Msg -> Element Msg
template html =
    Element.column [ height fill, width fill, Element.spaceEvenly, Background.color Const.ui.themeColor.background, Font.family [ Font.monospace ] ]
        [ el [ Element.centerX, Element.padding 10, Font.color Const.ui.themeColor.accentBackground, Font.size Const.ui.fontSize.large ] <|
            text "Elm-Tic-Tac"
        , html
        , footer
        ]


footer : Element msg
footer =
    Element.column
        [ Element.centerX, Element.padding 10, Element.spacing 5 ]
        [ Element.paragraph [ Element.centerX, Font.size Const.ui.fontSize.small ]
            [ text "The "
            , Element.newTabLink [ Font.underline ] { url = "https://github.com/DanielCardonaRojas/elm-tic-tac", label = text "code" }
            , text " for this game is open sourced and written in Elm"
            ]
        , el [ Element.centerX, Font.size Const.ui.fontSize.small ] <| text "© 2018 Daniel Cardona Rojas"
        ]



-- Scenes


class =
    Element.htmlAttribute << Html.Attributes.class


rematch : Model -> Element Msg
rematch model =
    el [] <| text "Rematch"


playerScore : Int -> Bool -> String -> Player -> Element msg
playerScore score disabled title player =
    el
        [ Element.alignTop
        , class (player |> Player.toString |> String.toLower)
        ]
    <|
        text <|
            title
                ++ ": "
                ++ String.fromInt score
