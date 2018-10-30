module Style.Process exposing (..)

import Element exposing (Attribute, Element)


type alias Styler a msg =
    a -> List (Attribute msg)


with : Styler a msg -> List a -> List (Attribute msg)
with styler ls =
    List.foldr (\s attrs -> styler s ++ attrs) [] ls


combined : List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg)
combined =
    List.append


adding : Attribute msg -> List (Attribute msg) -> List (Attribute msg)
adding attr ls =
    ls ++ [ attr ]
