module Style.Size exposing
    ( Edge(..)
    , Size(..)
    , length
    , padding
    , paddingEach
    , paddingXY
    , scaledFont
    , spacing
    )

import Constants as Const
import Element exposing (Attribute, Device, DeviceClass(..), Element)


type Size
    = Small
    | Normal
    | Large
    | XLarge


scaledFont : Int -> DeviceClass -> Int
scaledFont factor device =
    let
        scaled baseSize =
            Element.modular (toFloat baseSize) 1.25 >> round
    in
    case device of
        Phone ->
            scaled Const.ui.fontSize.xlarge factor

        Tablet ->
            scaled Const.ui.fontSize.xlarge factor

        Desktop ->
            scaled Const.ui.fontSize.medium factor

        BigDesktop ->
            scaled Const.ui.fontSize.medium factor


type Edge
    = Left
    | Right
    | Top
    | Bottom


length : Size -> Int
length s =
    case s of
        Small ->
            Const.ui.spacing.small

        Normal ->
            Const.ui.spacing.normal

        Large ->
            Const.ui.spacing.large

        XLarge ->
            Const.ui.spacing.xLarge


paddingEach : Edge -> Size -> Attribute msg
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


paddingXY : Size -> Size -> Attribute msg
paddingXY s1 s2 =
    Element.paddingXY (length s1) (length s2)


padding : Size -> Attribute msg
padding s =
    paddingXY s s


spacing : Size -> Attribute msg
spacing s =
    length s
        |> Element.spacing
