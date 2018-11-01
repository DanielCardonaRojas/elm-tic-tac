module BoardTests exposing (..)

import Data.Board as Board
import Data.Player as Player exposing (Player(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


-- Board Test


testCannotPlayOnBlockedBoard : Test
testCannotPlayOnBlockedBoard =
    test "Cannot make a move on board thats been blocked." <|
        \_ ->
            let
                board =
                    Board.cubic 3
                        |> Board.lock
                        |> Board.play3D 0 { column = 1, row = 1, player = PlayerX }
            in
            Expect.equal True <| List.isEmpty (Board.moves board)
