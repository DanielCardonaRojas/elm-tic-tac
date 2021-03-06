module Data.Board exposing
    ( Board
    , BoardIndex
    , Spot
    , decode
    , emptySpots
    , enabled
    , encode
    , lock
    , locked
    , make
    , moves
    , play
    , size
    , spots
    , tilesAt
    , toggleLock
    , unlock
    , won
    )

import Data.Move as Move exposing (Move, Move3D, Positioned, Positioned3D)
import Data.Player as Player exposing (Player)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe



-- This module does calculations on moves but does not dictate the game play itself


type alias BoardIndex =
    Int


type alias Spot =
    Positioned3D {}


type alias Spots =
    List Spot


type Board
    = Board
        { size : Int -- this determines the board will be NxNxN
        , moves : List Move3D
        , disabledBoards : List Int
        }


encode : Board -> Encode.Value
encode (Board board) =
    Encode.object
        [ ( "size", Encode.int board.size )
        ]


decode : Decoder { size : Int }
decode =
    Decode.succeed (\s -> { size = s })
        |> required "size" Decode.int


moves : Board -> List Move3D
moves (Board board) =
    .moves board


size : Board -> Int
size (Board board) =
    .size board


spots : Board -> Int
spots (Board board) =
    board.size ^ 3


emptySpots : Board -> Int
emptySpots board =
    spots board - List.length (moves board)


locked : Board -> Bool
locked (Board board) =
    List.length board.disabledBoards >= board.size


tilesAt : BoardIndex -> Board -> List (Positioned { player : Maybe Player })
tilesAt k board =
    boardTiles (clamp 0 (size board - 1) k) <| board


boardTiles : BoardIndex -> Board -> List (Positioned { player : Maybe Player })
boardTiles idx (Board board) =
    let
        movesOnBoard =
            List.filter (\m -> m.board == idx) board.moves
                |> List.map (\m -> { column = m.column, row = m.row, player = Just m.player })

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


make : Int -> Board
make n =
    Board
        { size = n
        , moves = []
        , disabledBoards = []
        }


lock : Board -> Board
lock (Board board) =
    Board { board | disabledBoards = List.range 0 (board.size - 1) }


singleLock : BoardIndex -> Board -> Board
singleLock idx (Board board) =
    Board
        { board | disabledBoards = [ clamp 0 (board.size - 1) idx ] }


unlock : Board -> Board
unlock (Board board) =
    Board { board | disabledBoards = [] }


enabled : Bool -> Board -> Board
enabled bool board =
    if bool then
        unlock board

    else
        lock board


toggleLock : Board -> Board
toggleLock board =
    if locked board then
        unlock board

    else
        lock board


play : BoardIndex -> Move -> Board -> Board
play idx move (Board board) =
    Board
        { board
            | moves =
                if not <| locked (Board board) then
                    (Move.fromMoveInBoard idx move :: board.moves)
                        |> List.uniqueBy Move.positioned3DTuple

                else
                    board.moves
        }



-- Internal
-- Obtain a projection through a plane in cube


verticalColumnBoard : Board -> Int -> List Move
verticalColumnBoard (Board board) k =
    board.moves
        |> List.filter (\m -> m.column == k)
        |> List.map alongColumn


verticalRowBoard : Board -> Int -> List Move
verticalRowBoard (Board board) k =
    board.moves
        |> List.filter (\m -> m.row == k)
        |> List.map alongRow


fromVerticalColumnBoard : Int -> List Move -> List Move3D
fromVerticalColumnBoard k boardMoves =
    let
        -- Undo operations of projecting along column
        undoProjection m =
            { player = m.player
            , column = k
            , row = m.column
            , board = m.row
            }
    in
    List.map undoProjection boardMoves


fromVerticalRowBoard : Int -> List Move -> List Move3D
fromVerticalRowBoard k boardMoves =
    let
        -- Undo operations of projecting along column
        undoProjection m =
            { player = m.player
            , column = m.row
            , row = k
            , board = m.row
            }
    in
    List.map undoProjection boardMoves


horizontalBoard : Board -> Int -> List Move
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


won : Board -> Maybe (List Move3D)
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

        diagonals =
            cubeDiagonals board.size

        onDiagonal : Spots -> Maybe (List Move3D)
        onDiagonal d =
            board.moves
                |> List.filterMap
                    (\m ->
                        if List.member (Move.positioned3D m) d then
                            Just m

                        else
                            Nothing
                    )
                |> Just
                |> Maybe.filter (\mvs -> List.length mvs == board.size)
                |> Maybe.filter (\mvs -> samePlayer mvs)
    in
    winOnVerticalColumnBoard
        |> Maybe.or winOnVerticalRowBoard
        |> Maybe.or winOnHorizontalBoard
        |> Maybe.or (onDiagonal <| diagonals.d1)
        |> Maybe.or (onDiagonal <| diagonals.d2)
        |> Maybe.or (onDiagonal <| diagonals.d3)
        |> Maybe.or (onDiagonal <| diagonals.d4)



-- | Determine if there is a sequence of winning moves


samePlayer : List { r | player : Player } -> Bool
samePlayer mvs =
    List.groupWhile (\m1 m2 -> m1.player == m2.player) mvs
        |> List.length
        |> (\l -> l == 1)


checkpass : Int -> List Move -> Maybe (List Move)
checkpass n boardMoves =
    Just boardMoves
        |> Maybe.filter (\mvs -> List.length mvs == n)
        |> Maybe.filter (\mvs -> samePlayer mvs)


didWin : Int -> List Move -> Maybe (List Move)
didWin n boardMoves =
    let
        vertical k =
            List.filter (\mv -> mv.column == k) boardMoves
                |> checkpass n

        horizontal i =
            List.filter (\mv -> mv.row == i) boardMoves
                |> checkpass n

        diagonal1 =
            let
                d1 =
                    List.range 0 (n - 1)
                        |> List.map2 (\x y -> { column = x, row = y }) (List.range 0 (n - 1))
            in
            List.filter (\m -> List.member (Move.positioned m) d1) boardMoves
                |> checkpass n
                |> List.singleton

        diagonal2 =
            let
                d1 =
                    List.range 0 (n - 1)
                        |> List.reverse
                        |> List.map2 (\x y -> { column = x, row = y }) (List.range 0 (n - 1))
            in
            List.filter (\m -> List.member (Move.positioned m) d1) boardMoves
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
        |> List.append diagonal1
        |> List.append diagonal2
        |> List.filter (\r -> r /= Nothing)
        |> List.head
        |> Maybe.join


cubeDiagonals : Int -> { d1 : Spots, d2 : Spots, d3 : Spots, d4 : Spots }
cubeDiagonals n =
    let
        increasingIdx =
            List.range 0 (n - 1)

        decreasingIdx =
            List.reverse increasingIdx

        gen l1 l2 =
            -- Z is always increasing on cube diagonals
            List.map3 (\x y z -> ( x, y, z )) l1 l2 increasingIdx
                |> List.map (\( x, y, z ) -> { column = x, row = y, board = z })

        d1 =
            gen increasingIdx increasingIdx

        d2 =
            gen decreasingIdx decreasingIdx

        d3 =
            gen increasingIdx decreasingIdx

        d4 =
            gen decreasingIdx increasingIdx
    in
    { d1 = d1, d2 = d2, d3 = d3, d4 = d4 }
