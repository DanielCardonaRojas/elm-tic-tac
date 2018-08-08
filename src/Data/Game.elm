module Data.Game
    exposing
        ( Game
        , Status(..)
        , cubic
        , enable
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


type Status
    = Winner Player (List (Positioned3D {}))
    | Tie
    | Playing


type alias Game =
    { board : Board Cubic
    , win : Maybe (List Move3D)
    }


cubic : Int -> Game
cubic n =
    { board = Board.lock <| Board.cubic n
    , win = Nothing
    }


status : Game -> Status
status game =
    let
        fullBoard =
            Board.emptySpots game.board == 0

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
    Board.play3D idx move game.board
        |> (\board -> { game | board = board })
        |> updateWin


updateWin : Game -> Game
updateWin game =
    Board.cubicWin game.board
        |> (\win -> { game | win = win })


lock : Game -> Game
lock game =
    { game | board = Board.lock game.board }


unlock : Game -> Game
unlock game =
    { game | board = Board.unlock game.board }


enable : Bool -> Game -> Game
enable bool game =
    if bool then
        unlock game
    else
        lock game
