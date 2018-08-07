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
        -- Remote
        NewGameSingle n ->
            Return.singleton { model | player = Maybe.map Player.switch model.player, game = Game.flat n }
                |> Return.map (\m -> { m | game = Game.enable (isCurrentPlayerTurn model) m.game })
                |> Return.map (\m -> { m | turn = Player.PlayerX })

        NewGameMulti n ->
            Return.singleton { model | player = Maybe.map Player.switch model.player, game = Game.cubic n }
                |> Return.map (\m -> { m | game = Game.enable (isCurrentPlayerTurn model) m.game })
                |> Return.map (\m -> { m | turn = Player.PlayerX })
                |> Debug.log "NewGameMulti"

        Opponent move idx ->
            Return.singleton model.game
                |> Return.map (Game.update move idx)
                |> Return.map Game.unlock
                |> Return.map (\g -> { model | game = g, turn = Player.switch model.turn })

        SetOponent p ->
            Return.singleton { model | opponent = Just p }
                |> Return.map (\m -> { m | isReady = Utils.shouldStartGame m })

        -- Local
        PlayAgainSingle n ->
            Return.singleton { model | game = Game.flat n, player = Maybe.map Player.switch model.player }
                |> Return.map (\m -> { m | turn = Player.PlayerX })
                |> Return.map (\m -> { m | game = Game.enable (isCurrentPlayerTurn model) m.game })
                |> Return.command (SocketIO.emit "rematch" <| Board.encode <| Board.flat n)

        PlayAgainMulti n ->
            Return.singleton { model | game = Game.cubic n, player = Maybe.map Player.switch model.player }
                |> Return.map (\m -> { m | turn = Player.PlayerX })
                |> Return.map (\m -> { m | game = Game.enable (isCurrentPlayerTurn model) m.game })
                |> Return.command (SocketIO.emit "rematch" <| Board.encode <| Board.cubic n)

        Play move idx ->
            -- TODO: Emit movement including board index
            Return.singleton model.game
                |> Return.map (Game.update move idx)
                |> Return.map Game.lock
                |> Return.map (\g -> { model | game = g, turn = Player.switch model.turn })
                |> Return.command (SocketIO.emit "move" <| Move.encode3D <| Move.fromMoveInBoard idx move)

        SetPlayer p ->
            Return.singleton { model | player = Just p }
                |> Return.map (\m -> { m | isReady = Utils.shouldStartGame m })
                |> Return.map (\m -> { m | game = Game.enable (p == model.turn) m.game })
                |> Return.command (SocketIO.emit "join" <| Player.encode p)

        NoOp ->
            model ! []


init : ( Model, Cmd Msg )
init =
    Return.singleton Model.default
        --|> Return.command (SocketIO.connect "http://localhost:8000")
        |> Return.command (SocketIO.connect "")
        |> Return.command (SocketIO.listen "move")
        |> Return.command (SocketIO.listen "join")
        |> Return.command (SocketIO.listen "rematch")


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

                "rematch" ->
                    Board.decode
                        |> Decode.map
                            (\config ->
                                if config.cubic then
                                    NewGameMulti config.size
                                else
                                    NewGameSingle config.size
                            )

                _ ->
                    Decode.fail "No registered decoder"
    in
    Sub.batch
        [ SocketIO.decodeMessage socketIODecoder NoOp
        ]


isCurrentPlayerTurn : Model -> Bool
isCurrentPlayerTurn model =
    Maybe.map (\p -> p == model.turn) model.player
        |> Maybe.withDefault False
