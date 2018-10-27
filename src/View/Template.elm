module View.Template exposing (primary)

import Constants as Const
import Element exposing (Attribute, Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Font as Font


primary : Element msg -> Element msg
primary html =
    Element.column [ height fill, width fill, Element.spaceEvenly, Background.color Const.ui.themeColor.background, Font.family [ Font.monospace ] ]
        [ el [ Element.centerX, Element.padding 10, Font.color Const.ui.themeColor.accentBackground, Font.size Const.ui.fontSize.large ] <|
            text "Elm-Tic-Tac"
        , html
        , footer
        ]


footer : Element msg
footer =
    Element.column
        [ Element.padding Const.ui.spacing.small
        , Background.color Const.colors.lightGray
        , width fill
        ]
        [ Element.paragraph [ Element.centerX, Font.size Const.ui.fontSize.small, Font.center ]
            [ text "The "
            , Element.newTabLink [ Font.underline ] { url = "https://github.com/DanielCardonaRojas/elm-tic-tac", label = text "code" }
            , text " for this game is open sourced and written in Elm"
            ]
        , el [ Element.centerX, Font.size Const.ui.fontSize.small ] <| text "© 2018 Daniel Cardona Rojas"
        ]
