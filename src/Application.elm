module Application exposing (main)

import Data.Board as Board exposing (Board, Cubic, Flat)
import Data.Game as Game exposing (Game)
import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)
import Html
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import Ports.Echo as Echo
import Ports.LocalStorage as LocalStorage
import Ports.SocketIO as SocketIO
import Return
import Utils
import View


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = View.view
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Switch mode ->
            { model | game = createGame mode } ! []

        Play move idx ->
            -- TODO: Emit movement including board index
            Return.singleton model.game
                |> Return.map (Game.update move idx)
                |> Return.map Game.lock
                |> Return.map (\g -> { model | game = g, turn = Player.switch model.turn })
                |> Return.command (SocketIO.emit "move" <| Move.encode3D <| Move.fromMoveInBoard idx move)

        Opponent move idx ->
            Return.singleton model.game
                |> Return.map (Game.update move idx)
                |> Return.map Game.unlock
                |> Return.map (\g -> { model | game = g, turn = Player.switch model.turn })

        SetPlayer p ->
            Return.singleton { model | player = Just p }
                |> Return.map (\m -> { m | isReady = Utils.shouldStartGame m })
                |> Return.map (\m -> { m | game = Game.enable (p == model.turn) m.game })
                |> Return.command (SocketIO.emit "join" <| Player.encode p)

        SetOponent p ->
            Return.singleton { model | opponent = Just p }
                |> Return.map (\m -> { m | isReady = Utils.shouldStartGame m })

        NoOp ->
            model ! []


init : ( Model, Cmd Msg )
init =
    Return.singleton Model.default
        |> Return.command (SocketIO.connect "http://localhost:8000")
        |> Return.command (SocketIO.listen "move")
        |> Return.command (SocketIO.listen "join")


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
                                    |> Debug.log "socket.io move"
                            )

                "join" ->
                    Player.decode
                        |> Decode.map SetOponent

                _ ->
                    Decode.fail "No registered decoder"
    in
    Sub.batch
        [ SocketIO.decodeMessage socketIODecoder NoOp
        ]



-- Helpers


createGame : Msg.Mode -> Game
createGame mode =
    case mode of
        Msg.SingleBoard n ->
            Game.flat n

        Msg.MultiBoard n ->
            Game.cubic n
