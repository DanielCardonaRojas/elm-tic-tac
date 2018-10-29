module View.Game exposing (render, score)

import Constants as Const
import Data.Game as Game exposing (Game, Status(..))
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Input as Input
import Msg exposing (Msg(..))
import View.Board as Board
import View.Style as Style exposing (style)


render : List (Attribute Msg) -> Game -> Player -> Element Msg
render attributes game player =
    let
        button txt msg =
            Input.button (Element.centerX :: style Style.Button)
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
    el
        (Element.alignTop :: width (Element.maximum 200 fill) :: style (Style.PlayerScore player <| not disabled))
        (text <| title ++ ": " ++ String.fromInt points)
