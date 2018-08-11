module View.Game exposing (render)

import Data.Game as Game exposing (Game, Status(..))
import Data.Player as Player exposing (Player)
import View.Board as Board
import Msg exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

render : Game -> Player -> Html Msg
render game player =
    let
        winTitle winner =
                (\p ->
                    if p == winner then
                        h2 [] [ text "You win" ]
                    else
                        h2 [] [ text "You loose" ]
                )
                player

        lockedBoard =
            renderBoard (Game.lock game) player

        board =
            renderBoard game player
    in
    case game.status of
        Winner p moves ->
            div [ class "game" ]
                [ winTitle p
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
                [ board
                ]


renderBoard : Game -> Player -> Html Msg
renderBoard game nextPlayer =
    Board.render3D
        (\pos ->
            Play { column = pos.column, row = pos.row, player = nextPlayer } pos.board
        )
        game.board