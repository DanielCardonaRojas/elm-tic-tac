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
import Style.Process as Process exposing (Styler)
import Style.Rules as Style exposing (Rules(..))


singleBoard : Board -> List (Element msg) -> Styler Rules msg -> Element msg
singleBoard board tiles style =
    List.groupsOf (Board.size board) tiles
        |> List.map (Element.row [ Element.centerX, Element.spacing Const.ui.spacing.xxSmall ])
        |> Element.column (Element.centerY :: Element.spacing Const.ui.spacing.xxSmall :: style Style.Board)


render3D : (Positioned3D {} -> Maybe msg) -> Board -> Styler Rules msg -> Element msg
render3D tagger board style =
    List.range 0 (Board.size board - 1)
        |> List.map (\idx -> renderNthBoard tagger board idx style)
        |> Element.column (width fill :: height fill :: style Style.BoardCube)


renderFlat : (Positioned3D {} -> Maybe msg) -> Board -> BoardIndex -> Styler Rules msg -> Element msg
renderFlat tagger board selected style =
    Element.column []
        [ renderNthBoard tagger board selected style
        ]


renderNthBoard : (Positioned3D {} -> Maybe msg) -> Board -> BoardIndex -> Styler Rules msg -> Element msg
renderNthBoard tagger board n style =
    let
        renderTile pos =
            move tagger pos style
    in
    Board.tilesAt n board
        |> List.map (Move.from2D n)
        |> List.map renderTile
        |> (\els -> singleBoard board els style)


maybeIf : Bool -> a -> Maybe a
maybeIf b v =
    if b then
        Just v

    else
        Nothing


move : (Positioned3D {} -> Maybe msg) -> Positioned3D { player : Maybe Player } -> Styler Rules msg -> Element msg
move emptyTagger m style =
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
