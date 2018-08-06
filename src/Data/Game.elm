module Data.Game
    exposing
        ( Game
        , Mode(..)
        , cubic
        , enable
        , flat
        , lock
        , unlock
        , update
        )

import Data.Board as Board exposing (Board, Cubic, Flat)
import Data.Move as Move exposing (Move, Move3D)


type Mode
    = Simple (Board Flat)
    | Advanced (Board Cubic)


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
