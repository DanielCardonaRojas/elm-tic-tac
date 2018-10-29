module View.Board exposing (render2D, render3D)

--import Html exposing (..)
--import Html.Events as Events exposing (..)

import Constants as Const
import Data.Board as Board exposing (..)
import Data.Move as Move exposing (Move, Positioned, Positioned3D)
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Input as Input
import Html
import Html.Attributes
import List.Extra as List
import View.Style as Style exposing (style)


singleBoard : Board a -> List (Element msg) -> Element msg
singleBoard board tiles =
    List.groupsOf (Board.size board) tiles
        |> List.map (Element.row [ Element.centerX, Element.spacing Const.ui.spacing.xxSmall ])
        |> Element.column (Element.centerY :: Element.spacing Const.ui.spacing.xxSmall :: style Style.Board)


render3D : (Positioned3D {} -> msg) -> Board Cubic -> Element msg
render3D tagger board =
    let
        renderTile =
            move (Board.locked board) tagger

        tiles n =
            Board.tilesAt n board
                |> List.map (Move.from2D n)

        renderNthBoard n =
            tiles n
                |> List.map renderTile
                |> singleBoard board
    in
    List.range 0 (Board.size board - 1)
        |> List.map renderNthBoard
        |> Element.column (width fill :: height fill :: style Style.BoardCube)


render2D : (Positioned {} -> msg) -> Board Flat -> Element msg
render2D tagger board =
    let
        renderTile =
            move (Board.locked board) (\xyz -> tagger <| Move.positioned xyz)

        tiles =
            Board.tiles board
                |> List.map (Move.from2D 0)
    in
    List.map renderTile tiles
        |> singleBoard board


maybeIf : Bool -> a -> Maybe a
maybeIf b v =
    if b then
        Just v
    else
        Nothing


move : Bool -> (Positioned3D {} -> msg) -> Positioned3D { player : Maybe Player } -> Element msg
move enabled emptyTagger m =
    let
        size =
            50

        button msg =
            Input.button
                (style <| Style.BoardTile m.player)
                { label = Element.none
                , onPress = maybeIf enabled msg
                }
    in
    button <| emptyTagger (Move.positioned3D m)
