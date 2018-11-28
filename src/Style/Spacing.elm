module Style.Spacing exposing (Edge(..), Spacing(..), length, padding, paddingEach, paddingXY, spacing)

import Constants as Const
import Element exposing (Attribute, Element)


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
