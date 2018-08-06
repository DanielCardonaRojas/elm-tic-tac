module Application exposing (main)

import Data.Board as Board exposing (Board, Cubic, Flat)
import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)
import Html
import Json.Decode as Decode exposing (Decoder)
import Model exposing (..)
import Msg exposing (Msg(..))
import Ports.Echo as Echo
import Ports.LocalStorage as LocalStorage
import Ports.SocketIO as SocketIO
import Return
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
                |> Return.map (updateGame move idx)
                |> Return.map lockGame
                |> Return.map (\g -> { model | game = g, turn = Player.switch model.turn })
                |> Return.command (SocketIO.emit "move" <| Move.encode3D <| Move.fromMoveInBoard idx move)

        Opponent move idx ->
            Return.singleton model.game
                |> Return.map (updateGame move idx)
                |> Return.map unlockGame
                |> Return.map (\g -> { model | game = g, turn = Player.switch model.turn })

        SetPlayer p ->
            Return.singleton { model | player = Just p }
                |> Return.map
                    (\m ->
                        { m
                            | game =
                                if p == model.turn then
                                    unlockGame m.game
                                else
                                    lockGame m.game
                        }
                    )

        NoOp ->
            model ! []


updateGame : Move -> Int -> Game -> Game
updateGame move idx game =
    case game of
        Simple board ->
            Board.play2D move board
                |> Simple

        Advanced board ->
            Board.play3D idx move board
                |> Advanced


lockGame : Game -> Game
lockGame game =
    case game of
        Simple board ->
            Board.lock board
                |> Simple

        Advanced board ->
            Board.lock board
                |> Advanced


unlockGame : Game -> Game
unlockGame game =
    case game of
        Simple board ->
            Board.unlock board
                |> Simple

        Advanced board ->
            Board.unlock board
                |> Advanced


createGame : Msg.Mode -> Game
createGame mode =
    case mode of
        Msg.SingleBoard n ->
            Simple <| Board.flat n

        Msg.MultiBoard n ->
            Advanced <| Board.cubic n


init : ( Model, Cmd Msg )
init =
    Return.singleton Model.default
        |> Return.command (SocketIO.connect "http://localhost:8000")
        |> Return.command (SocketIO.listen "move")


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        socketIODecoder str =
            case str of
                _ ->
                    Move.decode3D
                        |> Decode.map
                            (\m ->
                                Opponent (Move.as2D m) m.board
                                    |> Debug.log "socket.io move"
                            )
    in
    Sub.batch
        [ SocketIO.decodeMessage socketIODecoder NoOp
        ]
