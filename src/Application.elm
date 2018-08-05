module Application exposing (main)

import Data.Board as Board
import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)
import Html
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
            Return.singleton { model | game = updateGame move idx model.game, turn = Player.switch model.turn }
                |> Return.command (SocketIO.emit "move" <| Move.encode3D <| Move.fromMoveInBoard idx move)

        Opponent move idx ->
            { model | game = updateGame move idx model.game, turn = Player.switch model.turn } ! []

        SetPlayer p ->
            { model | player = Just p } ! []


updateGame : Move -> Int -> Game -> Game
updateGame move idx game =
    case game of
        Simple board ->
            Board.play2D move board
                |> Simple

        Advanced board ->
            Board.play3D idx move board
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
        |> Return.command (SocketIO.connect "")


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
