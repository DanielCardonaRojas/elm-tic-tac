module Utils exposing (..)

import Data.Board as Board exposing (Board)
import Data.Move as Move exposing (Move)
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (..)


shouldStartGame : Model -> Bool
shouldStartGame model =
    Just (\p1 p2 -> p1 /= p2)
        |> Maybe.andMap model.player
        |> Maybe.andMap model.opponent
        |> Maybe.withDefault False


enablingGame : Bool -> Model -> Model
enablingGame bool m =
    { m
        | game =
            if bool then
                unlockGame m.game
            else
                lockGame m.game
    }


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
