module Data.Game
    exposing
        ( Game
        , Mode(..)
        , Status(..)
        , cubic
        , enable
        , flat
        , lock
        , status
        , unlock
        , update
        )

-- This module is a thin wrapper around Board module
-- to help this be a little easiear to handle in the app

import Data.Board as Board exposing (Board, Cubic, Flat)
import Data.Move as Move exposing (Move, Move3D, Positioned3D)
import Data.Player as Player exposing (Player)
import Maybe.Extra as Maybe


type Mode
    = Simple (Board Flat)
    | Advanced (Board Cubic)


type Status
    = Winner Player (List (Positioned3D {}))
    | Tie
    | Playing


type alias Game =
    { mode : Mode
    , win : Maybe (List Move3D)
    }


cubic : Int -> Game
cubic n =
    { mode = Advanced <| Board.lock <| Board.cubic n
    , win = Nothing
    }


flat : Int -> Game
flat n =
    { mode = Simple <| Board.lock <| Board.flat n
    , win = Nothing
    }


status : Game -> Status
status game =
    let
        fullBoard =
            case game.mode of
                Simple board ->
                    Board.emptySpots board == 0

                Advanced board ->
                    Board.emptySpots board == 0

        player =
            Maybe.map List.head game.win
                |> Maybe.join
                |> Maybe.map .player

        moves =
            game.win
                |> Maybe.map (List.map Move.positioned3D)
    in
    case ( player, moves, fullBoard ) of
        ( Just p, Just moves, _ ) ->
            Winner p moves

        ( _, _, True ) ->
            Tie

        ( _, _, _ ) ->
            Playing


update : Move -> Int -> Game -> Game
update move idx game =
    case game.mode of
        Simple board ->
            Board.play2D move board
                |> (\mode -> { game | mode = Simple mode })
                |> updateWin

        Advanced board ->
            Board.play3D idx move board
                |> (\mode -> { game | mode = Advanced mode })
                |> updateWin


updateWin : Game -> Game
updateWin game =
    case game.mode of
        Simple board ->
            Board.flatWin board
                |> (\win -> { game | mode = Simple board, win = win })

        Advanced board ->
            Board.cubicWin board
                |> (\win -> { game | mode = Advanced board, win = win })


lock : Game -> Game
lock game =
    case game.mode of
        Simple board ->
            Board.lock board
                |> (\mode -> { game | mode = Simple mode })

        Advanced board ->
            Board.lock board
                |> (\mode -> { game | mode = Advanced mode })


unlock : Game -> Game
unlock game =
    case game.mode of
        Simple board ->
            Board.unlock board
                |> (\mode -> { game | mode = Simple mode })

        Advanced board ->
            Board.unlock board
                |> (\mode -> { game | mode = Advanced mode })


enable : Bool -> Game -> Game
enable bool game =
    if bool then
        unlock game
    else
        lock game
