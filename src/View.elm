module View exposing (view)

--import Html exposing (Html)

import Constants as Const
import Data.Player as Player exposing (Player)
import Element exposing (Element, el, fill, height, text, width)
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
            Element.column [ Element.spaceEvenly, Element.centerX ]
                [ Setup.connection (model.room == Nothing) str
                , maybe roomInfo model.room
                ]
                |> template

        PlayerChoose ->
            Element.column []
                [ Setup.playerPicker model
                , maybe roomInfo model.room
                ]
                |> template

        GamePlay ->
            Element.column [ class "center" ]
                [ leftPortion model
                , maybe (Game.render model.game >> Element.html) model.player
                , rightPortion model
                ]
                |> template

        Rematch ->
            rematch model
                |> template


leftPortion : Model -> Element Msg
leftPortion model =
    Element.column [ class "left" ]
        [ maybe (\p -> playerScore (Tuple.first model.score) (model.turn /= p) "You" p) model.player ]


rightPortion : Model -> Element Msg
rightPortion model =
    Element.column [ class "right" ]
        [ maybe (\p -> playerScore (Tuple.second model.score) (model.turn /= p) "Opponent" p) model.opponent
        , maybe roomInfo model.room
        ]


maybe f =
    Maybe.map f >> Maybe.withDefault Element.none


roomInfo : String -> Element Msg
roomInfo roomName =
    el [ class "room-connection" ] <|
        text <|
            "Connected to room: "
                ++ roomName


template : Element Msg -> Element Msg
template html =
    Element.column [ height fill, width fill, Element.spaceEvenly, Background.color Const.ui.themeColor.background, Font.family [ Font.monospace ] ]
        [ el [ Element.centerX, Element.padding 10, Font.color Const.ui.themeColor.accentBackground, Font.size Const.ui.fontSize.large ] <|
            text "Elm-Tic-Tac"
        , el [ width fill ] html
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
    el [ class "rematch" ] <| text "Rematch"


playerScore : Int -> Bool -> String -> Player -> Element msg
playerScore score disabled title player =
    el
        (class "score"
            :: class (player |> Player.toString |> String.toLower)
            :: (if disabled then
                    [ class "disabled" ]
                else
                    []
               )
        )
    <|
        text <|
            title
                ++ ": "
                ++ String.fromInt score
