module Style.Process exposing (..)

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


adding : Attribute msg -> List (Attribute msg) -> List (Attribute msg)
adding =
    addingWhen True


addingWhen : Bool -> Attribute msg -> List (Attribute msg) -> List (Attribute msg)
addingWhen b attr ls =
    if b then
        ls ++ [ attr ]
    else
        ls


when : Bool -> Attribute msg -> Attribute msg -> Attribute msg
when b left right =
    if b then
        left
    else
        right
