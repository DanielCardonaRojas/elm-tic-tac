module View.Board exposing (render2D, render3D)

--import Html exposing (..)
--import Html.Events as Events exposing (..)

import Constants as Const
import Data.Board as Board exposing (..)
import Data.Move as Move exposing (Move, Positioned, Positioned3D)
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import List.Extra as List


singleBoard : Board a -> List (Element msg) -> Element msg
singleBoard board tiles =
    List.groupsOf (Board.size board) tiles
        |> List.map (Element.row [ Element.centerX, Element.spacing Const.ui.spacing.xxSmall ])
        |> Element.column [ Element.centerY, skew 35, Element.spacing Const.ui.spacing.xxSmall ]


class =
    Element.htmlAttribute << Html.Attributes.class


skew : Int -> Attribute msg
skew degrees =
    Element.htmlAttribute <| Html.Attributes.style "transform" ("skew(" ++ String.fromInt degrees ++ "deg)")


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
        |> Element.column
            [ width fill
            , height fill
            , Element.spacing Const.ui.spacing.normal
            , Element.rotate <| degrees -20
            ]


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
        tileClass =
            Maybe.map (String.append "tile player" << String.toLower << Player.toString) m.player
                |> Maybe.withDefault "tile"

        playerColor player =
            if player == PlayerX then
                Const.colors.red
            else
                Const.colors.blue

        tileColor =
            Maybe.map playerColor m.player |> Maybe.withDefault Const.colors.lightGray

        size =
            50

        button msg =
            Input.button
                [ class tileClass
                , width <| Element.px size
                , height <| Element.px size
                , Background.color tileColor
                , Border.color <| Element.rgba 1 1 1 0
                ]
                { label = Element.none
                , onPress = maybeIf enabled msg
                }
    in
    button <| emptyTagger (Move.positioned3D m)
