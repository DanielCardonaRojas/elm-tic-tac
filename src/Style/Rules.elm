module Style.Rules exposing
    ( Rules(..)
    , style
    , styled
    )

import Constants as Const
import Data.Player as Player exposing (Player(..))
import Element exposing (Attribute, Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Style.Process as Style exposing (Styler)
import Style.Spacing as Spacing exposing (..)
import Style.Theme as Theme



-- Top level styling rules


type Rules
    = Title
    | Subtitle
    | Paragragh
    | Button Bool
    | Panel
    | Section
    | Label
    | Textfield
      -- Detailed
    | Setup
    | SetupButton Bool
    | PlayerScore Player Bool -- Player and isCurrentPlayer flag.
    | PlayerButton Player Bool
    | Board
    | BoardCube
    | Template
    | TemplateFooter
    | TemplateTitle
    | BoardTile (Maybe Player)


style : Rules -> List (Attribute msg)
style =
    styled Nothing



--| Use recursion to reuse styles using Style.process functions.


styled : Maybe Device -> Rules -> List (Attribute msg)
styled device st =
    case st of
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

        -- Setup screens styles
        Setup ->
            Style.asA Panel (styled device)
                |> Style.combined (Theme.for Theme.Surface)

        SetupButton enabled ->
            Style.asA (Button enabled) (styled device)
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
            Style.asA Label (styled device)
                |> Style.adding Font.center
                |> Style.adding (Background.color <| playerColor player)
                |> Style.addingWhen (not enabled) (Element.alpha 0.3)

        PlayerButton player enabled ->
            Style.asA (Button enabled) (styled device)
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

        _ ->
            []



-- Helpers


skew : Int -> Attribute msg
skew degrees =
    Element.htmlAttribute <| Html.Attributes.style "transform" ("skew(" ++ String.fromInt degrees ++ "deg)")


playerColor player =
    if player == PlayerX then
        Const.colors.red

    else
        Const.colors.blue
