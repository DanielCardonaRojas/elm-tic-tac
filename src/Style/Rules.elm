module Style.Rules exposing
    ( Rules(..)
    , styled
    )

import Constants as Const
import Data.Game as Game exposing (ViewMode(..))
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Device, DeviceClass(..), Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Style.Process as Style exposing (Styler)
import Style.Size as Size exposing (..)
import Style.Theme as Theme



-- Top level styling rules


type Rules
    = Title
    | Subtitle
    | Paragragh
    | Button Bool
      -- Debug
    | Class String
      -- Padding and spacing
    | Padded Size
    | Spaced Size
    | Skew Int
      -- Generic
    | Panel
    | Section
    | Label
    | Textfield
      -- Font
    | SmallFont
      -- Detailed
    | Setup
    | SetupButton Bool
    | PlayerScore Player Bool -- Player and isCurrentPlayer flag.
    | PlayerButton Player Bool
    | BoardCube
    | Board -- Overall styles to board
    | BoardSingle ViewMode -- Dims of board depending of rendering mode.
    | BoardTab
    | Template
    | TemplateFooter
    | TemplateTitle
    | BoardTile (Maybe Player)
    | BoardTilePreview (Maybe Player)



--| Use recursion to reuse styles using Style.process functions.


styled : { width : Int, height : Int } -> Styler Rules msg
styled dims st =
    let
        device =
            Element.classifyDevice dims

        fontSize n =
            (.class >> scaledFont n) device

        self =
            styled dims

        maxDim =
            min dims.width dims.height
    in
    case st of
        Padded size ->
            [ padding size ]

        Spaced size ->
            [ spacing size ]

        Skew angle ->
            [ skew angle ]

        Button enabled ->
            [ padding Small
            , Border.rounded 5
            ]
                |> Style.addingWhen (not enabled) (Element.alpha 0.3)

        Panel ->
            [ spacing Small
            , paddingXY Small Normal
            , Border.rounded 10
            ]

        Section ->
            [ paddingXY Normal Small
            ]

        Label ->
            [ padding Small
            ]

        Textfield ->
            [ Theme.on Theme.Primary
            ]

        SmallFont ->
            [ Font.size <| fontSize -2
            ]

        Class str ->
            [ class str ]

        -- Setup screens styles
        Setup ->
            Style.asA Panel self
                |> Style.combined (Theme.for Theme.Surface)
                |> Style.adding (Font.size <| fontSize 1)

        SetupButton enabled ->
            Style.asA (Button enabled) self
                |> Style.adding (Background.color Const.colors.lightSalmon)
                |> Style.adding (Theme.on Theme.Surface)

        -- Board Styles
        Board ->
            self <| Class "myboard"

        BoardSingle mode ->
            [ Element.width <| Element.px <| singleBoardSize device.class maxDim mode
            , Element.height <| Element.px <| singleBoardSize device.class maxDim mode
            , Element.centerX
            ]
                |> Style.combined (self <| Class "xoboard")

        BoardCube ->
            [ spacing Normal
            , Element.rotate <| degrees -20
            , Element.centerX
            , Element.width <| Element.maximum 200 Element.fill
            , Element.width Element.fill
            , Element.height Element.fill
            ]

        BoardTab ->
            [ Background.color <| Element.rgba 0 0 0 0.7
            , Font.color <| Element.rgb 1 1 1
            , spacing Large
            , paddingXY Normal Normal
            ]

        BoardTile m ->
            [ Background.color (Maybe.map playerColor m |> Maybe.withDefault Const.colors.lightGray)
            , Border.color <| Element.rgba 1 1 1 0
            , Element.width <| Element.fillPortion 1
            , Element.height <| Element.fillPortion 1
            ]

        BoardTilePreview m ->
            Style.asA (BoardTile m) self
                |> Style.adding (Element.alpha 0.3)

        PlayerScore player enabled ->
            Style.asA Label self
                |> Style.adding Font.center
                |> Style.adding (Background.color <| playerColor player)
                |> Style.adding (Font.size <| fontSize 1)
                |> Style.addingWhen (not enabled) (Element.alpha 0.3)

        PlayerButton player enabled ->
            Style.asA (Button enabled) self
                |> Style.adding (Background.color <| playerColor player)

        -- Template Styles
        Template ->
            [ Theme.color Theme.Background
            , Font.family [ Font.monospace ]
            , Font.size <| fontSize 1
            ]

        TemplateFooter ->
            [ Background.color Const.colors.lightGray
            , padding Small
            , Font.size <| fontSize -1
            , Font.center
            ]

        TemplateTitle ->
            [ Font.color Const.ui.themeColor.accentBackground
            , Font.size <| fontSize 4
            ]

        _ ->
            []



-- Helpers


singleBoardSize deviceClass maxDim viewMode =
    let
        adjustCubicSize s =
            if viewMode == Cubic then
                s / 1.5

            else
                s

        scaleSize factor =
            (factor * toFloat maxDim) |> adjustCubicSize |> round
    in
    case deviceClass of
        Phone ->
            scaleSize 0.38

        Tablet ->
            scaleSize 0.35

        Desktop ->
            scaleSize 0.3

        BigDesktop ->
            scaleSize 0.2


skew : Int -> Attribute msg
skew degrees =
    Element.htmlAttribute <| Html.Attributes.style "transform" ("skew(" ++ String.fromInt degrees ++ "deg)")


class : String -> Attribute msg
class name =
    Element.htmlAttribute <| Html.Attributes.class name


playerColor player =
    if player == PlayerX then
        Const.colors.red

    else
        Const.colors.blue
