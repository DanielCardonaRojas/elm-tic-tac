module Subscriptions exposing (subscriptions)

import Browser.Events
import Data.Board as Board exposing (Board)
import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)
import Json.Decode as Decode exposing (Decoder)
import Model exposing (..)
import Msg exposing (Msg(..))
import Ports.SocketIO as SocketIO


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        socketIODecoder str =
            case str of
                "move" ->
                    Move.decode3D
                        |> Decode.map
                            (\m ->
                                Opponent (Move.as2D m) m.board
                             --|> Debug.log "socket.io move"
                            )

                "joinedGame" ->
                    Decode.string
                        |> Decode.map
                            (always SetupReady)

                "chosePlayer" ->
                    Player.decode
                        |> Decode.map SetOponent

                "rematch" ->
                    Board.decode
                        |> Decode.map (NewGame << .size)

                "socketid" ->
                    Decode.string
                        |> Decode.map SocketID

                "newGame" ->
                    Decode.string
                        |> Decode.map SetRoom

                _ ->
                    Decode.fail "No registered decoder"
    in
    Sub.batch
        [ SocketIO.decodeMessage socketIODecoder NoOp
        , Browser.Events.onResize WindowResize
        ]
