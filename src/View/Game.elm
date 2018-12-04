module View.Game exposing (render, score)

import Constants as Const
import Data.Game as Game exposing (Game, Status(..))
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Input as Input
import Msg exposing (Msg(..))
import Style.Process as Style exposing (Styler)
import Style.Rules as Rules exposing (Rules(..))
import View.Board as Board


render : List (Attribute Msg) -> Styler Rules Msg -> Game -> Player -> Element Msg
render attributes style game player =
    let
        button txt msg =
            Input.button
                (style (Rules.Button True)
                    |> Style.adding Element.centerX
                )
                { label = el [ Element.centerX ] <| text txt
                , onPress = Just msg
                }
    in
    case game.status of
        Winner p moves ->
            Element.column attributes
                [ button "Rematch" (PlayAgain <| Game.size game)
                , renderBoard (Game.lock game) player style
                ]

        Tie ->
            Element.column attributes
                [ el [ Element.centerX ] <| text "Tie"
                , button "Play Again" (PlayAgain 3)
                , renderBoard (Game.lock game) player style
                ]

        Playing ->
            Element.column attributes
                [ renderBoard game player style
                ]


renderBoard : Game -> Player -> Styler Rules Msg -> Element Msg
renderBoard game nextPlayer style =
    Board.render3D
        (\pos ->
            if game.turn == nextPlayer then
                Just <| Play { column = pos.column, row = pos.row, player = nextPlayer } pos.board

            else
                Nothing
        )
        game.board
        style
        |> el [ Element.centerX, Element.centerY ]


score : Int -> Bool -> String -> Player -> Styler Rules msg -> Element msg
score points disabled title player style =
    el
        (Element.alignTop :: (width <| Element.maximum 300 fill) :: style (Rules.PlayerScore player <| not disabled))
        (text <| title ++ ": " ++ String.fromInt points)
