module Data.Board
    exposing
        ( Board
        , BoardIndex
        , Cubic
        , Flat
        , cubic
        , cubicWin
        , flat
        , flatWin
        , lock
        , locked
        , moves
        , play2D
        , play3D
        , size
        , tiles
        , toggleLock
        , unlock
        )

import Data.Move as Move exposing (Move, Positioned, Positioned3D)
import Data.Player as Player exposing (Player)
import List.Extra as List
import Maybe.Extra as Maybe


-- This module does calculations on moves


type alias BoardMove =
    Positioned3D { player : Player }


type alias BoardIndex =
    Int


type Flat
    = Flat


type Cubic
    = Cubic


type Board a
    = Board
        { size : Int -- this determines the board will be nxn
        , cubic : Bool -- We can determiner the amount of boards
        , moves : List BoardMove
        , enabled : Bool
        }


moves : Board a -> List BoardMove
moves (Board board) =
    .moves board


size : Board a -> Int
size (Board board) =
    .size board


locked : Board a -> Bool
locked (Board board) =
    .enabled board


tiles : BoardIndex -> Board a -> List (Positioned { player : Maybe Player })
tiles idx (Board board) =
    let
        movesOnBoard =
            List.filter (\m -> m.board == idx) board.moves
                |> List.map
                    (\m ->
                        { column = m.column
                        , row = m.row
                        , player = Just m.player
                        }
                    )

        allPositions =
            let
                enum =
                    List.range 0 (board.size - 1)
            in
            enum
                |> List.andThen
                    (\row ->
                        enum
                            |> List.andThen
                                (\col ->
                                    [ { column = col, row = row, player = Nothing } ]
                                )
                    )
    in
    allPositions
        |> List.map
            (\p ->
                List.filter (Move.equallyPositioned p) movesOnBoard
                    |> List.head
                    |> Maybe.withDefault p
            )


flat : Int -> Board Flat
flat n =
    Board
        { size = n
        , cubic = True
        , moves = []
        , enabled = True
        }


cubic : Int -> Board Cubic
cubic n =
    Board
        { size = n
        , cubic = True
        , moves = []
        , enabled = True
        }


lock : Board a -> Board a
lock (Board board) =
    Board { board | enabled = False }


unlock : Board a -> Board a
unlock (Board board) =
    Board { board | enabled = True }


toggleLock : Board a -> Board a
toggleLock (Board board) =
    Board { board | enabled = not board.enabled }


play : BoardIndex -> Move -> Board a -> Board a
play idx move (Board board) =
    Board
        { board | moves = Move.fromMoveInBoard idx move :: board.moves }


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
        |> List.filter (\m -> m.board == k)
        |> List.map alongColumn


horizontalSlice : Board a -> Int -> List Move
horizontalSlice (Board board) k =
    board.moves
        |> List.filter (\m -> m.board == k)
        |> List.map alongRow


alongColumn : BoardMove -> Move
alongColumn pos =
    { player = pos.player
    , column = pos.row
    , row = pos.board
    }


alongRow : BoardMove -> Move
alongRow pos =
    { player = pos.player
    , column = pos.column
    , row = pos.row
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
