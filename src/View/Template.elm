module View.Template exposing (primary, withItems)

import Constants as Const
import Element exposing (Attribute, Device, DeviceClass(..), Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Font as Font
import Style.Process as Style exposing (Styler)
import Style.Rules as Rules exposing (Rules)


primary : Device -> Styler Rules msg -> Element msg -> Element msg
primary device styler html =
    withItems device styler Element.none Element.none html


withItems : Device -> Styler Rules msg -> Element msg -> Element msg -> Element msg -> Element msg
withItems device styler topLeft topRight content =
    Element.column
        (height fill :: width fill :: Element.spaceEvenly :: styler Rules.Template)
        [ header styler topLeft topRight
        , body content
        , footer styler |> hideWhen (device.class == Phone || device.class == Tablet)
        ]


body : Element msg -> Element msg
body content =
    el
        [ Element.paddingXY Const.ui.spacing.normal 0
        , Element.centerX
        , Element.centerY
        , width fill
        , height fill
        ]
        content


header : Styler Rules msg -> Element msg -> Element msg -> Element msg
header styler topLeft topRight =
    Element.row
        (styler Rules.Section
            |> Style.adding (width fill)
            |> Style.adding Element.spaceEvenly
        )
        [ el [ Element.alignLeft, width fill ] topLeft
        , el (width fill :: Element.centerX :: Font.center :: styler Rules.TemplateTitle) <| text "Elm-Tic-Tac"
        , el [ Element.alignRight, width fill ] topRight
        ]


footer : Styler Rules msg -> Element msg
footer styler =
    Element.column
        (width fill :: styler Rules.TemplateFooter)
        [ Element.paragraph [ Element.centerX ]
            [ text "The "
            , Element.newTabLink [ Font.underline ] { url = "https://github.com/DanielCardonaRojas/elm-tic-tac", label = text "code" }
            , text " for this game is open sourced and written in Elm"
            ]
        , el [ Element.centerX ] <| text "© 2018 Daniel Cardona Rojas"
        ]


hideWhen : Bool -> Element msg -> Element msg
hideWhen flag element =
    if flag then
        Element.none

    else
        element
