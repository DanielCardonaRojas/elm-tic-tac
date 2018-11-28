module Msg exposing (Msg(..))

import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)


type Msg
    = Play Move Int -- Local player move
    | Opponent Move Int -- Remote player move
    | SocketID String
    | SetRoom String -- Set by the socket.io server after creating game
    | SelectRoom String -- Set by player to join room
    | CreateGame String -- Create socket.io room on server
    | RoomSetup String -- Input when user types
    | NewGame Int
    | SetPlayer Player
    | SetOponent Player
    | PlayAgain Int
    | SetupReady
    | WindowResize Int Int
    | NoOp
