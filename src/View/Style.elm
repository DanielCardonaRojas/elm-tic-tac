module View.Style exposing (Styles(..), style)

import Constants as Const
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


type Styles
    = Setup
    | SetupButton
    | Button
    | PlayerScore Player Bool -- Player and isCurrentPlayer flag.
    | PlayerButton Player
    | Game
    | Board
    | BoardCube
    | Template
    | TemplateFooter
    | TemplateTitle
    | BoardTile (Maybe Player)


style : Styles -> List (Attribute msg)
style st =
    case st of
        -- Setup screens styles
        Setup ->
            [ Background.color Const.ui.themeColor.paneBackground
            , Element.spacing Const.ui.spacing.small
            , Element.paddingXY Const.ui.spacing.small Const.ui.spacing.normal
            , Font.color Const.colors.gray
            , Border.rounded 10
            , Element.centerX
            ]

        SetupButton ->
            [ Element.padding Const.ui.spacing.small
            , Background.color <| Const.ui.themeColor.paneButtonBackground
            ]

        Button ->
            [ Element.padding Const.ui.spacing.small
            ]

        -- Board Styles
        Board ->
            [ skew 35
            ]

        BoardCube ->
            [ Element.spacing Const.ui.spacing.normal
            , Element.rotate <| degrees -20
            ]

        BoardTile m ->
            [ Background.color (Maybe.map playerColor m |> Maybe.withDefault Const.colors.lightGray)
            , Border.color <| Element.rgba 1 1 1 0
            , Element.width <| Element.px 50
            , Element.height <| Element.px 50
            ]

        PlayerScore player enabled ->
            [ Element.padding Const.ui.spacing.small
            , Font.center
            , Background.color <| playerColor player
            , when enabled (Element.alpha 1.0) (Element.alpha 0.3)
            ]

        PlayerButton player ->
            [ Element.padding Const.ui.spacing.small
            , Background.color <| playerColor player
            ]

        Game ->
            []

        -- Template Styles
        Template ->
            [ Background.color Const.ui.themeColor.background
            , Font.family [ Font.monospace ]
            ]

        TemplateFooter ->
            [ Background.color Const.colors.lightGray
            , Element.padding Const.ui.spacing.small
            , Font.size Const.ui.fontSize.small
            , Font.center
            ]

        TemplateTitle ->
            [ Element.padding Const.ui.spacing.small
            , Font.color Const.ui.themeColor.accentBackground
            , Font.size Const.ui.fontSize.large
            ]



-- Helpers


skew : Int -> Attribute msg
skew degrees =
    Element.htmlAttribute <| Html.Attributes.style "transform" ("skew(" ++ String.fromInt degrees ++ "deg)")


playerColor player =
    if player == PlayerX then
        Const.colors.red
    else
        Const.colors.blue


when : Bool -> a -> a -> a
when b left right =
    if b then
        left
    else
        right
