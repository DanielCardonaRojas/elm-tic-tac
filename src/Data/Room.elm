module Data.Room exposing (..)

import Json.Encode as Encode exposing (Value)


type alias Room o =
    { name : String
    , data : o
    }


name : String -> d -> Room d
name title data =
    Room title data


encode_ : Room o -> (o -> Value) -> Value
encode_ room encoder =
    Encode.object
        [ ( "data", encoder room.data )
        , ( "room", Encode.string room.name )
        ]


encode : String -> Value -> Value
encode room data =
    Encode.object
        [ ( "data", data )
        , ( "room", Encode.string room )
        ]
