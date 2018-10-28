module View.Game exposing (render, score)

import Constants as Const
import Data.Game as Game exposing (Game, Status(..))
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Msg exposing (Msg(..))
import View.Board as Board


class =
    Element.htmlAttribute << Html.Attributes.class


render : List (Attribute Msg) -> Game -> Player -> Element Msg
render attributes game player =
    let
        button txt msg =
            Input.button
                [ Element.centerX
                , Element.padding Const.ui.spacing.small
                , Background.color <| Const.ui.themeColor.paneButtonBackground
                ]
                { label = el [ Element.centerX ] <| text txt
                , onPress = Just msg
                }
    in
    case game.status of
        Winner p moves ->
            Element.column attributes
                [ button "Rematch" (PlayAgain <| Game.size game)
                , renderBoard (Game.lock game) player
                ]

        Tie ->
            Element.column attributes
                [ el [ Element.centerX ] <| text "Tie"
                , button "Play Again" (PlayAgain 3)
                , renderBoard (Game.lock game) player
                ]

        Playing ->
            Element.column attributes
                [ renderBoard game player
                ]


renderBoard : Game -> Player -> Element Msg
renderBoard game nextPlayer =
    Board.render3D
        (\pos ->
            Play { column = pos.column, row = pos.row, player = nextPlayer } pos.board
        )
        game.board
        |> el [ Element.centerX, Element.centerY ]


score : Int -> Bool -> String -> Player -> Element msg
score points disabled title player =
    let
        playerColor =
            if player == PlayerX then
                Const.colors.red
            else
                Const.colors.blue
    in
    el
        [ Element.alignTop
        , Background.color playerColor
        , Element.padding Const.ui.spacing.small
        , Font.center
        , width (Element.maximum 200 fill)
        , if disabled then
            Element.alpha 0.3
          else
            Element.alpha 1.0
        ]
        (text <| title ++ ": " ++ String.fromInt points)
