module BoardTests exposing (..)

import Data.Board as Board
import Data.Player as Player exposing (Player(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo "Implement our first test. See http://package.elm-lang.org/packages/elm-community/elm-test/latest for how to do this!"



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
