module Data.Game exposing
    ( Game
    , Status(..)
    , ViewMode(..)
    , enable
    , lock
    , make
    , rematch
    , size
    , switchTurn
    , unlock
    , update
    , updateSelected
    )

-- This module is a thin wrapper around Board module
-- to help this be a little easiear to handle in the app

import Data.Board as Board exposing (Board, Spot)
import Data.Move as Move exposing (Move, Move3D, Positioned3D)
import Data.Player as Player exposing (Player(..))
import Maybe.Extra as Maybe


type Status
    = Winner Player (List (Positioned3D {}))
    | Tie
    | Playing


type ViewMode
    = Single Int
    | Cubic


type alias Game =
    { board : Board
    , win : Maybe ( Player, List Spot )
    , status : Status
    , turn : Player
    , viewMode : ViewMode
    }


make : Int -> Game
make n =
    { board = Board.make n
    , win = Nothing
    , status = Playing
    , turn = PlayerX
    , viewMode = Cubic
    }


rematch : Int -> Game -> Game
rematch n game =
    { game | board = Board.make n }


switchTurn : Game -> Game
switchTurn game =
    { game | turn = game.turn |> Player.switch }


update : Move -> Int -> Game -> Game
update move idx game =
    if move.player == game.turn then
        Board.play idx move game.board
            |> (\b ->
                    { game | board = b }
               )
            |> updateWin
            |> updateStatus
            |> switchTurn

    else
        game


updateSelected : Int -> Game -> Game
updateSelected idx game =
    { game | viewMode = Single idx }


lock : Game -> Game
lock game =
    { game | board = Board.lock game.board }


unlock : Game -> Game
unlock game =
    { game | board = Board.unlock game.board }


enable : Bool -> Game -> Game
enable bool game =
    { game | board = Board.enabled bool game.board }


size : Game -> Int
size game =
    Board.size game.board



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
    Board.won game.board
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
    Just (\x y -> ( x, y ))
        |> Maybe.andMap player
        |> Maybe.andMap spots
