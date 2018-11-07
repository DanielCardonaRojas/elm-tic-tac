module Application exposing (main)

import Basics.Extra exposing (..)
import Browser
import Browser.Dom as Dom
import Data.Board as Board exposing (Board)
import Data.Game as Game exposing (Game)
import Data.Move as Move exposing (Move)
import Data.Player as Player exposing (Player)
import Data.Room as Room
import Json.Encode as Encode exposing (Value)
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import Ports.SocketIO as SocketIO
import Respond exposing (Respond)
import Return
import Subscriptions
import Task
import View


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = View.view
        , subscriptions = Subscriptions.subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Remote
        NewGame n ->
            Return.singleton { model | game = Game.make n, player = model.opponent, opponent = model.player }
                |> Return.map (\m -> { m | game = Game.enable (isCurrentPlayerTurn model) m.game })

        Opponent move idx ->
            Return.singleton model.game
                |> Return.map (Game.unlock >> Game.update move idx)
                |> Return.map (\g -> { model | game = g })
                |> Return.map updateScoreFromGame

        SetOponent p ->
            Return.singleton { model | opponent = Just p }

        SetRoom name ->
            Return.singleton { model | room = Just name }

        -- Local
        -- MatchSetup msgs
        SelectRoom name ->
            Return.singleton { model | room = Just name, scene = PlayerChoose }
                |> Return.command (SocketIO.emit "joinGame" <| Encode.string name)

        CreateGame str ->
            Return.singleton model
                |> Return.command (SocketIO.emit "createGame" <| Encode.string str)

        RoomSetup str ->
            Return.singleton { model | scene = MatchSetup str }

        PlayAgain n ->
            Return.singleton { model | game = Game.make n, player = model.opponent, opponent = model.player }
                |> Return.map (\m -> { m | game = Game.enable (isCurrentPlayerTurn model) m.game })
                |> Return.effect_ (emitInRoom "rematch" <| Board.encode <| Board.make n)

        Play move idx ->
            Return.singleton model.game
                |> Return.map (Game.update move idx >> Game.lock)
                |> Return.map (\g -> { model | game = g })
                |> Return.map updateScoreFromGame
                |> Return.effect_ (emitInRoom "move" <| Move.encode3D <| Move.fromMoveInBoard idx move)

        -- PlayerChoose msgs
        SetPlayer p ->
            Return.singleton { model | player = Just p }
                |> Return.map (\m -> { m | scene = GamePlay })
                |> Return.map (\m -> { m | game = Game.enable (p == model.game.turn) m.game })
                |> Return.effect_ (emitInRoom "chosePlayer" <| Player.encode p)

        SetupReady ->
            Return.singleton { model | scene = PlayerChoose }

        SocketID str ->
            Return.singleton { model | socketId = Just str }

        WindowResize w h ->
            Return.singleton { model | windowSize = ( w, h ) }

        NoOp ->
            Return.singleton model


init : ( Model, Cmd Msg )
init =
    let
        viewPortToWindowSize vp =
            vp.scene
                |> (\s -> ( round s.width, round s.height ))
    in
    Return.singleton Model.default
        --|> Return.command (SocketIO.connect "http://localhost:8000")
        |> Return.command
            (Dom.getViewport
                |> Task.attempt
                    (Result.map (viewPortToWindowSize >> uncurry WindowResize) >> Result.withDefault NoOp)
            )
        |> Return.command (SocketIO.connect "")
        |> Return.command (SocketIO.listen "move")
        |> Return.command (SocketIO.listen "joinedGame")
        |> Return.command (SocketIO.listen "socketid")
        |> Return.command (SocketIO.listen "rematch")
        |> Return.command (SocketIO.listen "newGame")
        |> Return.command (SocketIO.listen "chosePlayer")



-- Helpers


isCurrentPlayerTurn : Model -> Bool
isCurrentPlayerTurn model =
    Maybe.map (\p -> p == model.game.turn) model.player
        |> Maybe.withDefault False


emitInRoom : String -> Value -> Respond Msg Model
emitInRoom event value =
    \model ->
        model.room
            |> Maybe.map (\r -> SocketIO.emit event (Room.encode r value))
            |> Maybe.withDefault Cmd.none


updateScore : Player -> Player -> ( Int, Int ) -> ( Int, Int )
updateScore currentPlayer player ( playerScore, opponentScore ) =
    if currentPlayer == player then
        ( playerScore + 1, opponentScore )
    else
        ( playerScore, opponentScore + 1 )


updateScoreFromGame : Model -> Model
updateScoreFromGame model =
    Just updateScore
        |> Maybe.andMap model.player
        |> Maybe.andMap (Maybe.map Tuple.first model.game.win)
        |> Maybe.andMap (Just model.score)
        |> Maybe.map (\score -> { model | score = score })
        |> Maybe.withDefault model
