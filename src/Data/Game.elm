module Data.Game
    exposing
        ( Game
        , Status(..)
        , enable
        , lock
        , make
        , unlock
        , update
        )

-- This module is a thin wrapper around Board module
-- to help this be a little easiear to handle in the app

import Data.Board as Board exposing (Board, Cubic, Flat, Spot)
import Data.Move as Move exposing (Move, Move3D, Positioned3D)
import Data.Player as Player exposing (Player)
import Maybe.Extra as Maybe


type Status
    = Winner Player (List (Positioned3D {}))
    | Tie
    | Playing


type alias Game =
    { board : Board Cubic
    , win : Maybe ( Player, List Spot )
    , status : Status
    }


make : Int -> Game
make n =
    { board = Board.lock <| Board.cubic n
    , win = Nothing
    , status = Playing
    }


update : Move -> Int -> Game -> Game
update move idx game =
    Board.play3D idx move game.board
        |> (\board -> { game | board = board })
        |> updateWin
        |> updateStatus


lock : Game -> Game
lock game =
    { game | board = Board.lock game.board }


unlock : Game -> Game
unlock game =
    { game | board = Board.unlock game.board }


enable : Bool -> Game -> Game
enable bool game =
    { game | board = Board.enabled bool game.board }



-- Internal


updateStatus : Game -> Game
updateStatus game =
    let
        fullBoard =
            Board.emptySpots game.board == 0
    in
    case ( game.win, fullBoard ) of
        ( Just ( p, moves ), _ ) ->
            { game | status = Winner p moves }

        ( _, True ) ->
            { game | status = Tie }

        ( _, _ ) ->
            { game | status = Playing }


updateWin : Game -> Game
updateWin game =
    Board.cubicWin game.board
        |> (\win -> { game | win = winning win })


winning : Maybe (List Move3D) -> Maybe ( Player, List Spot )
winning moves =
    let
        player =
            Maybe.map List.head moves
                |> Maybe.join
                |> Maybe.map .player

        spots =
            Maybe.map (List.map Move.positioned3D) moves
    in
    Just (,)
        |> Maybe.andMap player
        |> Maybe.andMap spots
