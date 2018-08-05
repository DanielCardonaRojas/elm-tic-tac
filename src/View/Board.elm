module View.Board exposing (render2D, render3D)

import Data.Board as Board exposing (..)
import Data.Move as Move exposing (Move, Positioned, Positioned3D)
import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (..)


render3D : (Positioned3D {} -> msg) -> Board Cubic -> Html msg
render3D tagger board =
    let
        renderEmptyTile =
            move tagger

        tiles n =
            Board.tiles n board
                |> List.map
                    (\pos2D ->
                        { x = pos2D.column, y = pos2D.row, z = 0, player = pos2D.player }
                    )

        renderBoard n =
            List.map renderEmptyTile (tiles n)
                |> div [ class "board" ]
    in
    List.range 0 (Board.size board - 1)
        |> List.map renderBoard
        |> div [ class "cube" ]


render2D : (Positioned {} -> msg) -> Board Flat -> Html msg
render2D tagger board =
    let
        renderEmptyTile =
            move (\xyz -> tagger { column = xyz.x, row = xyz.y })

        tiles =
            Board.tiles 0 board
                |> Debug.log "Tiles"
                |> List.map
                    (\pos2D ->
                        { x = pos2D.column, y = pos2D.row, z = 0, player = pos2D.player }
                    )
    in
    List.map renderEmptyTile tiles
        |> div [ class "board" ]


move : (Positioned3D {} -> msg) -> Positioned3D { player : Maybe Player } -> Html msg
move emptyTagger m =
    let
        attrs =
            if m.player /= Nothing then
                [ class "tile" ]
            else
                [ class "tile"
                , onClick <| emptyTagger (Move.positioned3D m)
                ]
    in
    button attrs
        [ Maybe.map Player.toString m.player
            |> Maybe.withDefault ""
            |> text
        ]
