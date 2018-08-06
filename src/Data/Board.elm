module Data.Board
    exposing
        ( Board
        , BoardIndex
        , Cubic
        , Flat
        , cubic
        , cubicWin
        , decode
        , emptySpots
        , encode
        , flat
        , flatWin
        , lock
        , locked
        , moves
        , play2D
        , play3D
        , size
        , spots
        , tiles
        , toggleLock
        , unlock
        )

import Data.Move as Move exposing (Move, Move3D, Positioned, Positioned3D)
import Data.Player as Player exposing (Player)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe


-- This module does calculations on moves but does not dictate the game play itself


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
        , moves : List Move3D
        , enabled : Bool
        }


encode : Board a -> Encode.Value
encode (Board board) =
    Encode.object
        [ ( "size", Encode.int board.size )
        , ( "cubic", Encode.bool board.cubic )
        ]


decode : Decoder { size : Int, cubic : Bool }
decode =
    Decode.map2 (\s c -> { size = s, cubic = c })
        Decode.int
        Decode.bool


moves : Board a -> List Move3D
moves (Board board) =
    .moves board


size : Board a -> Int
size (Board board) =
    .size board


spots : Board a -> Int
spots (Board board) =
    if board.cubic then
        board.size ^ 3
    else
        board.size ^ 2


emptySpots : Board a -> Int
emptySpots board =
    spots board - List.length (moves board)


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


flatWin : Board Flat -> Maybe (List Move3D)
flatWin board =
    won board


cubicWin : Board Cubic -> Maybe (List Move3D)
cubicWin board =
    won board



-- Helpers
-- Obtain a projection through a plane in cube


verticalColumnBoard : Board Cubic -> Int -> List Move
verticalColumnBoard (Board board) k =
    board.moves
        |> List.filter (\m -> m.column == k)
        |> List.map alongColumn


verticalRowBoard : Board Cubic -> Int -> List Move
verticalRowBoard (Board board) k =
    board.moves
        |> List.filter (\m -> m.row == k)
        |> List.map alongRow


fromVerticalColumnBoard : Int -> List Move -> List Move3D
fromVerticalColumnBoard k moves =
    let
        -- Undo operations of projecting along column
        undoProjection m =
            { player = m.player
            , column = k
            , row = m.column
            , board = m.row
            }
    in
    List.map undoProjection moves


fromVerticalRowBoard : Int -> List Move -> List Move3D
fromVerticalRowBoard k moves =
    let
        -- Undo operations of projecting along column
        undoProjection m =
            { player = m.player
            , column = m.row
            , row = k
            , board = m.row
            }
    in
    List.map undoProjection moves


horizontalBoard : Board Cubic -> Int -> List Move
horizontalBoard (Board board) k =
    board.moves
        |> List.filter (\m -> m.board == k)
        |> List.map Move.as2D


alongColumn : Move3D -> Move
alongColumn pos =
    { player = pos.player
    , column = pos.row
    , row = pos.board
    }


alongRow : Move3D -> Move
alongRow pos =
    { player = pos.player
    , column = pos.column
    , row = pos.board
    }


won : Board a -> Maybe (List Move3D)
won (Board board) =
    let
        filterNonEmpty ls =
            List.filter (\m -> m /= Nothing) ls
                |> List.head
                |> Maybe.join

        winOnHorizontalBoard : Maybe (List Move3D)
        winOnHorizontalBoard =
            List.range 0 (board.size - 1)
                |> List.map
                    (\boardIdx ->
                        horizontalBoard (Board board) boardIdx
                            |> didWin board.size
                            |> Maybe.map (List.map (Move.fromMoveInBoard boardIdx))
                    )
                |> filterNonEmpty

        winOnVerticalColumnBoard : Maybe (List Move3D)
        winOnVerticalColumnBoard =
            List.range 0 (board.size - 1)
                |> List.map
                    (\col ->
                        verticalColumnBoard (Board board) col
                            |> didWin board.size
                            |> Maybe.map (fromVerticalColumnBoard col)
                    )
                |> filterNonEmpty

        winOnVerticalRowBoard : Maybe (List Move3D)
        winOnVerticalRowBoard =
            List.range 0 (board.size - 1)
                |> List.map
                    (\col ->
                        verticalRowBoard (Board board) col
                            |> didWin board.size
                            |> Maybe.map (fromVerticalRowBoard col)
                    )
                |> filterNonEmpty

        winOnCubic =
            winOnVerticalColumnBoard
                |> Maybe.or winOnVerticalRowBoard
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
    let
        sizeN mvs =
            List.length mvs |> (\k -> k == n)

        allFromSamePlayer mvs =
            case List.head mvs of
                Just m ->
                    List.all (\mv -> mv.player == m.player) mvs

                Nothing ->
                    False
    in
    Just moves
        |> Maybe.andThen
            (\mvs ->
                if sizeN mvs && allFromSamePlayer mvs then
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
