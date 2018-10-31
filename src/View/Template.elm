module View.Template exposing (primary, withItems)

import Constants as Const
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Font as Font
import Style.Process as Style
import Style.Rules as Style exposing (style)


primary : Element msg -> Element msg
primary html =
    withItems Element.none Element.none html


withItems : Element msg -> Element msg -> Element msg -> Element msg
withItems topLeft topRight content =
    let
        header =
            Element.row
                (Style.asA Style.Section Style.element
                    |> Style.adding (width fill)
                    |> Style.adding Element.spaceEvenly
                )
                [ el [ Element.alignLeft, width fill ] topLeft
                , el (width fill :: Element.centerX :: Font.center :: style Style.TemplateTitle) <| text "Elm-Tic-Tac"
                , el [ Element.alignRight, width fill ] topRight
                ]

        main =
            el
                [ Element.paddingXY Const.ui.spacing.normal 0
                , Element.centerX
                , Element.centerY
                , width fill
                , height fill
                ]
                content
    in
    Element.column
        (height fill :: width fill :: Element.spaceEvenly :: style Style.Template)
        [ header
        , main
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
