module View.Board exposing (render3D, renderFlat)

--import Html exposing (..)
--import Html.Events as Events exposing (..)

import Constants as Const
import Data.Board as Board exposing (..)
import Data.Game as Game exposing (ViewMode(..))
import Data.Move as Move exposing (Move, Positioned, Positioned3D)
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Input as Input
import Html
import Html.Attributes
import List.Extra as List
import Style.Process as Style exposing (Styler)
import Style.Rules as Style exposing (Rules(..))
import Style.Size as Size exposing (Size(..))


singleBoard : Board -> List (Element msg) -> Styler Rules msg -> Element msg
singleBoard board tiles style =
    List.groupsOf (Board.size board) tiles
        |> List.map
            (Element.row
                [ Element.spacing Const.ui.spacing.xxSmall
                , width fill
                , height fill
                ]
            )
        |> Element.column
            (Element.spacing Const.ui.spacing.xxSmall
                :: height fill
                :: width fill
                :: style Board
            )


render3D : (Positioned3D {} -> Maybe msg) -> Board -> Styler Rules msg -> Element msg
render3D tagger board style =
    List.range 0 (Board.size board - 1)
        |> List.map (\idx -> renderNthBoard tagger board idx <| Style.addingRules ( Board, [ Skew 35, BoardSingle Cubic ] ) style)
        |> Element.column (style BoardCube)


renderFlat : (Positioned3D {} -> Maybe msg) -> (BoardIndex -> msg) -> Board -> BoardIndex -> Styler Rules msg -> Element msg
renderFlat moveTagger boardTagger board selected style =
    let
        oneBoard =
            style
                |> Style.addingRules ( Board, [ Class "SingleBoard", BoardSingle <| Single 0 ] )
                |> renderNthBoard moveTagger board selected

        boardPreview k =
            previewNthBoard board k style

        boardTab idx =
            Input.button (style BoardTab)
                { label = Element.text <| String.fromInt idx
                , onPress = Just <| boardTagger idx
                }

        boardSelector =
            List.range 0 (Board.size board - 1)
                |> List.map (\idx -> el [ Element.behindContent <| boardPreview idx ] <| boardTab idx)
                |> Element.column (Element.alignLeft :: (style <| Spaced Small))
    in
    Element.row
        (style (Spaced Normal)
            |> Style.combined [ width fill, height fill ]
        )
        [ boardSelector, oneBoard ]


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


previewNthBoard : Board -> BoardIndex -> Styler Rules msg -> Element msg
previewNthBoard board n style =
    let
        renderTile pos =
            el (style (BoardTile pos.player)) (text "")
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
    Input.button
        (style (BoardTile m.player))
        { label = Element.none
        , onPress = emptyTagger (Move.positioned3D m)
        }
