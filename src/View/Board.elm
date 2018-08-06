module View.Board exposing (render2D, render3D)

import Data.Board as Board exposing (..)
import Data.Move as Move exposing (Move, Positioned, Positioned3D)
import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)


boardLayout : Int -> Attribute msg
boardLayout n =
    let
        columnTemplate n =
            "repeat(" ++ toString n ++ ", 1fr)"
    in
    style
        [ ( "display", "grid" )
        , ( "grid-template-columns", columnTemplate n )
        , ( "height", "200px" )
        , ( "width", "200px" )
        , ( "margin-bottom", "50px" )
        ]


render3D : (Positioned3D {} -> msg) -> Board Cubic -> Html msg
render3D tagger board =
    let
        renderEmptyTile =
            move (Board.locked board) tagger

        tiles n =
            Board.tiles n board
                |> List.map
                    (\pos2D ->
                        { column = pos2D.column, row = pos2D.row, board = n, player = pos2D.player }
                    )

        renderBoard n =
            List.map renderEmptyTile (tiles n)
                |> div [ class "board", boardLayout <| Board.size board ]
    in
    List.range 0 (Board.size board - 1)
        |> List.map renderBoard
        |> div [ class "cube" ]


render2D : (Positioned {} -> msg) -> Board Flat -> Html msg
render2D tagger board =
    let
        renderEmptyTile =
            move (Board.locked board) (\xyz -> tagger <| Move.positioned xyz)

        tiles =
            Board.tiles 0 board
                |> List.map
                    (\pos2D ->
                        { column = pos2D.column, row = pos2D.row, board = 0, player = pos2D.player }
                    )
    in
    List.map renderEmptyTile tiles
        |> div [ class "board", boardLayout <| Board.size board ]


move : Bool -> (Positioned3D {} -> msg) -> Positioned3D { player : Maybe Player } -> Html msg
move enabled emptyTagger m =
    let
        attrs =
            if m.player /= Nothing then
                []
            else
                [ onClick <| emptyTagger (Move.positioned3D m)
                ]
    in
    button (class "tile" :: id (toString m) :: (disabled <| not enabled) :: attrs)
        [ Maybe.map Player.toString m.player
            |> Maybe.withDefault ""
            |> text
        ]