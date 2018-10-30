module Style.Rules
    exposing
        ( ColorStyle(..)
        , ElementStyle(..)
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


-- Top level styling rules


type Styles
    = Setup
    | SetupButton
    | PlayerScore Player Bool -- Player and isCurrentPlayer flag.
    | PlayerButton Player
    | Game
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
    | Pane
    | Textfield


type ColorStyle
    = Background
    | PlayerBackground Player
    | Primary
    | Secondary


type Spacing
    = Small
    | Normal
    | Large


type Edge
    = Left
    | Right
    | Top
    | Bottom


length : Spacing -> Int
length s =
    case s of
        Small ->
            Const.ui.spacing.small

        Normal ->
            Const.ui.spacing.normal

        Large ->
            Const.ui.spacing.large


paddingEach : Edge -> Spacing -> Attribute msg
paddingEach e s =
    let
        edges =
            { top = 0, right = 0, bottom = 0, left = 0 }
    in
    case e of
        Top ->
            { edges | top = length s } |> Element.paddingEach

        Bottom ->
            { edges | bottom = length s } |> Element.paddingEach

        Right ->
            { edges | right = length s } |> Element.paddingEach

        Left ->
            { edges | left = length s } |> Element.paddingEach


paddingXY : Spacing -> Spacing -> Attribute msg
paddingXY s1 s2 =
    Element.paddingXY (length s1) (length s2)


padding : Spacing -> Attribute msg
padding s =
    paddingXY s s


spacing : Spacing -> Attribute msg
spacing s =
    length s
        |> Element.spacing



-- Sub Stylers: Breaks up styling into more atomic substyler.


element : Styler ElementStyle msg
element e =
    case e of
        Button ->
            [ Element.padding Const.ui.spacing.small
            ]

        _ ->
            []


style : Styles -> List (Attribute msg)
style st =
    case st of
        -- Setup screens styles
        Setup ->
            [ Background.color Const.ui.themeColor.paneBackground
            , spacing Small
            , paddingXY Small Normal
            , Font.color Const.colors.gray
            , Border.rounded 10
            , Element.centerX
            ]

        SetupButton ->
            Style.with element [ Button ]
                |> Style.adding (Background.color <| Const.ui.themeColor.paneButtonBackground)

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
            [ padding Small
            , Font.center
            , Background.color <| playerColor player
            , when enabled (Element.alpha 1.0) (Element.alpha 0.3)
            ]

        PlayerButton player ->
            Style.with element [ Button ]
                |> Style.adding (Background.color <| playerColor player)

        Game ->
            []

        -- Template Styles
        Template ->
            [ Background.color Const.ui.themeColor.background
            , Font.family [ Font.monospace ]
            ]

        TemplateFooter ->
            [ Background.color Const.colors.lightGray
            , padding Small
            , Font.size Const.ui.fontSize.small
            , Font.center
            ]

        TemplateTitle ->
            [ padding Small
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
