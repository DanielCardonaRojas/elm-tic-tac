module Style.Theme exposing (..)

import Constants as Const
import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Font as Font


type ThemeColor
    = Background
    | Primary
    | Secondary


color : ThemeColor -> Attribute msg
color c =
    case c of
        Background ->
            Background.color Const.ui.themeColor.paneBackground

        _ ->
            Background.color Const.ui.themeColor.paneBackground


on : ThemeColor -> Attribute msg
on c =
    Font.color Const.colors.white
