module BoardTests exposing (..)

import Data.Board as Board
import Data.Game as Game
import Data.Player as Player exposing (Player(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


-- Board Test


testCantPlayOnBlockedBoard : Test
testCantPlayOnBlockedBoard =
    test "Cannot make a move on board thats been blocked." <|
        \_ ->
            let
                board =
                    Board.make 3
                        |> Board.lock
                        |> Board.play 0 { column = 1, row = 1, player = PlayerX }
            in
            Expect.equal True <| List.isEmpty (Board.moves board)


testCantPlayTile : Test
testCantPlayTile =
    test "Can't place move on tile more than once." <|
        \_ ->
            let
                board =
                    Board.make 3
                        |> Board.play 0 { column = 1, row = 1, player = PlayerX }
                        |> Board.play 0 { column = 1, row = 1, player = PlayerO }
            in
            Expect.equal 1 <| List.length (Board.moves board)


testCantPlayConsecutiveTimes : Test
testCantPlayConsecutiveTimes =
    test "Player can't play more than one time in a row" <|
        \_ ->
            let
                move =
                    { row = 1, column = 1, player = PlayerX }

                game =
                    Game.make 3
                        |> Game.update move 0
                        |> Game.update move 1
            in
            Expect.equal 1 <| List.length (Board.moves game.board)
