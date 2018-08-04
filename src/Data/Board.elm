module Data.Board
    exposing
        ( Board
        , Cubic
        , Flat
        , Positioned3D
        , cubic
        , cubicWin
        , flat
        , flatWin
        , moves
        , play2D
        , play3D
        )

import Data.Move as Move exposing (Move, Player)
import Maybe.Extra as Maybe


-- This module does calculations on moves


type Board a
    = Board
        { size : Int -- this determines the board will be nxn
        , cubic : Bool -- We can determiner the amount of boards
        , moves : List Positioned3D
        }


moves : Board a -> List Positioned3D
moves (Board board) =
    .moves board


type alias Positioned3D =
    { player : Player
    , x : Int
    , y : Int
    , z : Int
    }


positioned : BoardIndex -> Move -> Positioned3D
positioned idx move =
    { player = move.player
    , x = move.column
    , y = move.row
    , z = idx
    }


type alias BoardIndex =
    Int


type Flat
    = Flat


type Cubic
    = Cubic


flat : Int -> Board Flat
flat n =
    Board
        { size = n
        , cubic = True
        , moves = []
        }


cubic : Int -> Board Cubic
cubic n =
    Board
        { size = n
        , cubic = True
        , moves = []
        }


play : BoardIndex -> Move -> Board a -> Board a
play idx move (Board board) =
    Board
        { board | moves = positioned idx move :: board.moves }


play3D : BoardIndex -> Move -> Board Cubic -> Board Cubic
play3D =
    play


play2D : Move -> Board Flat -> Board Flat
play2D =
    play 0


flatWin : Board Flat -> Maybe (List Move)
flatWin board =
    won board


cubicWin : Board Cubic -> Maybe (List Move)
cubicWin board =
    won board



-- Helpers
-- Obtain a projection through a plane in cube


verticalSlice : Board Cubic -> Int -> List Move
verticalSlice (Board board) k =
    board.moves
        |> List.filter (\m -> m.z == k)
        |> List.map alongColumn


horizontalSlice : Board a -> Int -> List Move
horizontalSlice (Board board) k =
    board.moves
        |> List.filter (\m -> m.z == k)
        |> List.map alongRow


alongColumn : Positioned3D -> Move
alongColumn pos =
    { player = pos.player
    , column = pos.y
    , row = pos.z
    }


alongRow : Positioned3D -> Move
alongRow pos =
    { player = pos.player
    , column = pos.x
    , row = pos.y
    }


won : Board a -> Maybe (List Move)
won (Board board) =
    let
        winOnHorizontalBoard : Maybe (List Move)
        winOnHorizontalBoard =
            List.range 0 (board.size - 1)
                |> List.map
                    (horizontalSlice (Board board)
                        >> didWin board.size
                    )
                |> List.filter (\m -> m /= Nothing)
                |> List.head
                |> Maybe.join

        winOnVerticalBoard : Maybe (List Move)
        winOnVerticalBoard =
            List.range 0 (board.size - 1)
                |> List.map
                    (verticalSlice (Board board)
                        >> didWin board.size
                    )
                |> List.filter (\m -> m /= Nothing)
                |> List.head
                |> Maybe.join

        winOnCubic =
            winOnVerticalBoard
                |> Maybe.or winOnHorizontalBoard
    in
    case board.cubic of
        True ->
            winOnCubic

        False ->
            Nothing



-- | Determine if there is a sequence of winning moves


checkpass : Int -> List Move -> Maybe (List Move)
checkpass n moves =
    Just moves
        |> Maybe.andThen
            (\mvs ->
                if List.length mvs == n then
                    Just mvs
                else
                    Nothing
            )


didWin : Int -> List Move -> Maybe (List Move)
didWin n moves =
    let
        vertical k =
            List.filter (\mv -> mv.column == k) moves
                |> checkpass n

        horizontal i =
            List.filter (\mv -> mv.row == i) moves
                |> checkpass n

        diagonals =
            let
                d1 =
                    List.range 0 (n - 1)
                        |> List.map2 (\x y -> { column = x, row = y }) (List.range 0 (n - 1))
            in
            List.filter (\m -> List.member (Move.positioned m) d1) moves
                |> checkpass n
                |> List.singleton

        verticals =
            List.range 0 (n - 1)
                |> List.map vertical

        horizontals =
            List.range 0 (n - 1)
                |> List.map horizontal
    in
    verticals
        |> List.append horizontals
        |> List.append diagonals
        |> List.filter (\r -> r /= Nothing)
        |> List.head
        |> Maybe.join
