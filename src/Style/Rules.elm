module Style.Rules
    exposing
        ( ElementStyle(..)
        , Styles(..)
        , element
        , style
        )

import Constants as Const
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Style.Process as Style exposing (Styler)
import Style.Spacing as Spacing exposing (..)
import Style.Theme as Theme


-- Top level styling rules


type Styles
    = Setup
    | SetupButton
    | PlayerScore Player Bool -- Player and isCurrentPlayer flag.
    | PlayerButton Player
    | Board
    | BoardCube
    | Template
    | TemplateFooter
    | TemplateTitle
    | BoardTile (Maybe Player)



-- Reusable styling rules


type ElementStyle
    = Title
    | Subtitle
    | Paragragh
    | Button
    | Panel
    | Section
    | Label
    | Textfield



-- Sub Stylers: Breaks up styling into more atomic substyler.


element : Styler ElementStyle msg
element e =
    case e of
        Button ->
            [ padding Small
            , Border.rounded 5
            ]

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

        _ ->
            []


style : Styles -> List (Attribute msg)
style st =
    case st of
        -- Setup screens styles
        Setup ->
            Style.asA Panel element
                |> Style.combined (Theme.for Theme.Surface)

        SetupButton ->
            Style.asA Button element
                |> Style.adding (Background.color Const.colors.lightSalmon)
                |> Style.adding (Theme.on Theme.Surface)

        -- Board Styles
        Board ->
            [ skew 35
            ]

        BoardCube ->
            [ spacing Normal
            , Element.rotate <| degrees -20
            ]

        BoardTile m ->
            [ Background.color (Maybe.map playerColor m |> Maybe.withDefault Const.colors.lightGray)
            , Border.color <| Element.rgba 1 1 1 0
            , Element.width <| Element.px 50
            , Element.height <| Element.px 50
            ]

        PlayerScore player enabled ->
            Style.asA Label element
                |> Style.adding Font.center
                |> Style.adding (Background.color <| playerColor player)
                |> Style.addingWhen (not enabled) (Element.alpha 0.3)

        PlayerButton player ->
            Style.asA Button element
                |> Style.adding (Background.color <| playerColor player)

        -- Template Styles
        Template ->
            [ Theme.color Theme.Background
            , Font.family [ Font.monospace ]
            ]

        TemplateFooter ->
            [ Background.color Const.colors.lightGray
            , padding Small
            , Font.size Const.ui.fontSize.small
            , Font.center
            ]

        TemplateTitle ->
            [ Font.color Const.ui.themeColor.accentBackground
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
