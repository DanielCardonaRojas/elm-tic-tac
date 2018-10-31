module Style.Theme exposing (ThemeColor(..), color, for, on)

import Constants as Const
import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Font as Font


type ThemeColor
    = Background
    | Primary
    | Secondary
    | Surface


colorOf : ThemeColor -> Color
colorOf c =
    case c of
        Background ->
            Const.colors.gray

        Surface ->
            --Const.ui.themeColor.paneBackground
            Element.rgb255 0 151 167

        Primary ->
            Const.colors.green

        Secondary ->
            Element.rgb255 0 151 167


color : ThemeColor -> Attribute msg
color c =
    colorOf c |> Background.color


on : ThemeColor -> Attribute msg
on c =
    case c of
        Background ->
            Font.color Const.colors.white

        Surface ->
            Font.color Const.colors.white

        Primary ->
            Font.color Const.colors.gray

        _ ->
            Font.color Const.colors.white


for : ThemeColor -> List (Attribute msg)
for c =
    [ color c, on c ]
