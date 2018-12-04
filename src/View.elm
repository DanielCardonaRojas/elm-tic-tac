module View exposing (view)

--import Html exposing (Html)

import Constants as Const
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
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
    let
        roomInfo =
            maybe (Setup.roomInfo [ Element.alignRight ]) model.room

        styler =
            Model.styler model

        templateWithItems =
            Template.withItems (Model.device model) styler

        templatePrimary =
            Template.primary (Model.device model) styler
    in
    case model.scene of
        MatchSetup str ->
            Element.column [ Element.centerX, Element.centerY, width <| Element.minimum 500 fill ]
                [ Setup.connection (model.room == Nothing) str styler
                ]
                |> templateWithItems (el [ width fill ] <| text "") roomInfo

        PlayerChoose ->
            Element.column [ Element.centerX, Element.centerY ]
                [ Setup.playerPicker model
                ]
                |> templateWithItems (el [] Element.none) roomInfo

        GamePlay ->
            Element.column [ Element.centerX, Element.spaceEvenly, width fill, height fill ]
                [ score model
                , maybe (Game.render [ Element.centerX, Element.centerY ] styler model.game) model.player
                ]
                |> templatePrimary

        Rematch ->
            templatePrimary <| el [] (text "Rematch")


score : Model -> Element msg
score model =
    let
        gameScore : Int -> String -> Player -> Element msg
        gameScore s name p =
            Game.score s (model.game.turn /= p) name p (Model.styler model)
    in
    Element.row
        [ Element.centerX
        , Element.spaceEvenly
        , width fill
        ]
        [ maybe (gameScore (Tuple.first model.score) "You") model.player
        , maybe (gameScore (Tuple.second model.score) "Opponent") model.opponent
        ]


maybe f =
    Maybe.map f >> Maybe.withDefault Element.none
