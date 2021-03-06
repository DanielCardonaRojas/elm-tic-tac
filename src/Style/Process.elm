module Style.Process exposing
    ( Styler
    , adding
    , addingRule
    , addingRules
    , addingWhen
    , asA
    , combined
    , combiningWhen
    , omittingRules
    , when
    , with
    )

import Element exposing (Attribute, Element)


type alias Styler a msg =
    a -> List (Attribute msg)


with : Styler a msg -> List a -> List (Attribute msg)
with styler ls =
    List.foldr (\s attrs -> styler s ++ attrs) [] ls


asA : a -> Styler a msg -> List (Attribute msg)
asA v styler =
    with styler [ v ]


combined : List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
combined =
    List.append


combiningWhen : Bool -> List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
combiningWhen b attrs ls =
    if b then
        ls ++ attrs

    else
        ls


adding : Attribute msg -> List (Attribute msg) -> List (Attribute msg)
adding =
    addingWhen True


addingWhen : Bool -> Attribute msg -> List (Attribute msg) -> List (Attribute msg)
addingWhen b attr ls =
    combiningWhen b [ attr ] ls



-- | Give a styler returns a new styler that every time selector passed in the styles of rule will
-- also be applied.


omittingRules : List a -> Styler a msg -> Styler a msg
omittingRules blacklist styler =
    \r ->
        if List.member r blacklist then
            []

        else
            styler r


addingRule : ( a, a ) -> Styler a msg -> Styler a msg
addingRule ( selector, rule ) styler =
    \a ->
        styler a
            |> combiningWhen (a == selector) (styler rule)


addingRules : ( a, List a ) -> Styler a msg -> Styler a msg
addingRules ( selector, rule ) styler =
    \a ->
        styler a
            |> combiningWhen (a == selector) (List.map styler rule |> List.concat)


when : Bool -> Attribute msg -> Attribute msg -> Attribute msg
when b left right =
    if b then
        left

    else
        right
