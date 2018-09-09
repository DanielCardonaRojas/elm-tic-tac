port module Ports.SocketIO exposing (connect, decodeMessage, emit, listen, send)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


------------- COMMANDS  -----------------
-- Connect to socketio server with url


port connect : String -> Cmd msg


{-| Register to a particular event
-}
port listen : String -> Cmd msg


port emit_ : ( String, Encode.Value ) -> Cmd msg


emit : String -> Encode.Value -> Cmd msg
emit name value =
    emit_ ( name, value )


send : Encode.Value -> Cmd msg
send =
    emit "message"



------------- SUBSCRIPTIONS  -----------------


port on_ : (( String, Decode.Value ) -> msg) -> Sub msg


decodeMessage : (String -> Decoder msg) -> msg -> Sub msg
decodeMessage toDecoder default =
    let
        toMsg : ( String, Decode.Value ) -> msg
        toMsg t =
            Tuple.second t
                |> Decode.decodeValue (Tuple.first t |> toDecoder |> traceDecoder "SocketIO decode msg")
                |> Result.withDefault default
    in
    toMsg |> on_



-- Utils


traceDecoder : String -> Decoder msg -> Decoder msg
traceDecoder message decoder =
    Decode.value
        |> Decode.andThen
            (\value ->
                case Decode.decodeValue decoder value of
                    Ok decoded ->
                        Decode.succeed decoded

                    Err err ->
                        Decode.fail <| Decode.errorToString err
            )
