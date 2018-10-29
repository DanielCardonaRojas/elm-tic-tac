module View.Template exposing (primary)

import Constants as Const
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Font as Font
import View.Style as Style exposing (style)


primary : Element msg -> Element msg
primary html =
    Element.column
        (height fill :: width fill :: Element.spaceEvenly :: style Style.Template)
        [ el (Element.centerX :: style Style.TemplateTitle) <| text "Elm-Tic-Tac"
        , el
            [ Element.paddingXY Const.ui.spacing.normal 0
            , Element.centerX
            , Element.centerY
            , width fill
            , height fill
            ]
            html
        , footer
        ]


footer : Element msg
footer =
    Element.column
        (width fill :: style Style.TemplateFooter)
        [ Element.paragraph [ Element.centerX ]
            [ text "The "
            , Element.newTabLink [ Font.underline ] { url = "https://github.com/DanielCardonaRojas/elm-tic-tac", label = text "code" }
            , text " for this game is open sourced and written in Elm"
            ]
        , el [ Element.centerX ] <| text "© 2018 Daniel Cardona Rojas"
        ]
