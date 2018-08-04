module Application exposing (main)

import Data.Board as Board
import Data.Move as Move exposing (Move)
import Html
import Model exposing (..)
import Msg exposing (Msg(..))
import Ports.Echo as Echo
import Ports.LocalStorage as LocalStorage
import Ports.SocketIO as SocketIO
import View


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = View.view
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Switch mode ->
            ( { model | game = createGame mode }, Cmd.none )

        Play move idx ->
            ( { model | game = updateGame move idx model.game }, Cmd.none )

        Opponent move idx ->
            ( { model | game = updateGame move idx model.game }, Cmd.none )

        msg ->
            ( model, Cmd.none )


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
    Model.default ! []
