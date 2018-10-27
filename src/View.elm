module View exposing (view)

--import Html exposing (Html)

import Constants as Const
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import View.Game as Game
import View.Setup as Setup
import View.Template as Template


view model =
    Element.layout [] (viewElement model)


viewElement : Model -> Element Msg
viewElement model =
    case model.scene of
        MatchSetup str ->
            Element.column [ Element.centerX ]
                [ Setup.connection (model.room == Nothing) str
                , maybe (roomInfo [ Font.size Const.ui.fontSize.small ]) model.room
                ]
                |> Template.primary

        PlayerChoose ->
            Element.column [ Element.centerX ]
                [ Setup.playerPicker model
                , maybe (roomInfo [ Font.size Const.ui.fontSize.small ]) model.room
                ]
                |> Template.primary

        GamePlay ->
            Element.row [ class "gameplay", Element.centerX, width fill, height fill ]
                [ leftPortion model
                , centerPortion model
                , rightPortion model
                ]
                |> Template.primary

        Rematch ->
            Template.primary <| el [] (text "Rematch")


centerPortion : Model -> Element Msg
centerPortion model =
    maybe
        (Game.render
            [ width <| Element.fillPortion 2
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
        [ maybe (\p -> Game.score (Tuple.first model.score) (model.turn /= p) "You" p) model.player ]


rightPortion : Model -> Element Msg
rightPortion model =
    Element.column
        [ width <| Element.fillPortion 1
        , height fill
        , Element.padding Const.ui.spacing.normal
        ]
        [ maybe (\p -> Game.score (Tuple.second model.score) (model.turn /= p) "Opponent" p) model.opponent
        , maybe (roomInfo [ Element.alignBottom ]) model.room
        ]


maybe f =
    Maybe.map f >> Maybe.withDefault Element.none


roomInfo : List (Attribute Msg) -> String -> Element Msg
roomInfo attrs roomName =
    el attrs <| text ("Connected to room: " ++ roomName)


class =
    Element.htmlAttribute << Html.Attributes.class
