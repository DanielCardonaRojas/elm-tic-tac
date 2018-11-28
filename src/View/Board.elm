module View.Board exposing (render3D)

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
import Style.Rules as Style exposing (style)


singleBoard : Board -> List (Element msg) -> Element msg
singleBoard board tiles =
    List.groupsOf (Board.size board) tiles
        |> List.map (Element.row [ Element.centerX, Element.spacing Const.ui.spacing.xxSmall ])
        |> Element.column (Element.centerY :: Element.spacing Const.ui.spacing.xxSmall :: style Style.Board)


render3D : (Positioned3D {} -> Maybe msg) -> Board -> Element msg
render3D tagger board =
    List.range 0 (Board.size board - 1)
        |> List.map (renderNthBoard tagger board)
        |> Element.column (width fill :: height fill :: style Style.BoardCube)


renderFlat : (Positioned3D {} -> Maybe msg) -> Board -> BoardIndex -> Element msg
renderFlat tagger board selected =
    Element.column []
        [ renderNthBoard tagger board selected
        ]


renderNthBoard : (Positioned3D {} -> Maybe msg) -> Board -> BoardIndex -> Element msg
renderNthBoard tagger board n =
    let
        renderTile =
            move tagger
    in
    Board.tilesAt n board
        |> List.map (Move.from2D n)
        |> List.map renderTile
        |> singleBoard board


maybeIf : Bool -> a -> Maybe a
maybeIf b v =
    if b then
        Just v

    else
        Nothing


move : (Positioned3D {} -> Maybe msg) -> Positioned3D { player : Maybe Player } -> Element msg
move emptyTagger m =
    let
        size =
            50

        button msg =
            Input.button
                (style <| Style.BoardTile m.player)
                { label = Element.none
                , onPress = msg
                }
    in
    button <| emptyTagger (Move.positioned3D m)
