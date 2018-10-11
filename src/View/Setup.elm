module View.Setup exposing (connection, playerPicker)

import Data.Player as Player exposing (Player(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))


connection : Bool -> String -> Html Msg
connection enabled room =
    div [ class "setup" ]
        [ h3 [] [ text "Create a new match or join one" ]
        , input [ onInput (String.trim >> String.toLower >> RoomSetup), placeholder "Enter room name", disabled <| not enabled ] []
        , button [ onClick <| CreateGame room, disabled <| not enabled ] [ text "Create Game" ]
        , button [ onClick <| SelectRoom room, disabled <| not enabled ] [ text "Join" ]
        , p [] [ text "Elm-Tic-Tac is a two player online 3D tic tac toe game" ]
        ]


playerPicker : Model -> Html Msg
playerPicker model =
    let
        activeAttr player =
            if Maybe.map (\p -> p == player) model.player |> Maybe.withDefault False then
                [ class "is-active" ]
            else
                []

        enabledFor player =
            Maybe.map (\p -> p /= player) model.opponent
                |> Maybe.withDefault True

        segment player =
            button
                (playerClass player :: (onClick <| SetPlayer player) :: (disabled <| not <| enabledFor player) :: activeAttr player)
                [ text <| "Player" ++ Player.toString player ]
    in
    div [ class "picker" ]
        [ span [] [ text "Choose a player" ]
        , segment Player.PlayerX
        , segment Player.PlayerO
        ]


playerClass : Player -> Attribute msg
playerClass p =
    "player" ++ Player.toString p |> String.toLower |> class


playerPicker_ : Player -> Player -> Html Msg
playerPicker_ player opponent =
    let
        activeAttr p =
            if p == player then
                [ class "is-active" ]
            else
                []

        enabledFor p =
            p /= player

        segment player_ =
            button
                (playerClass player_ :: (onClick <| SetPlayer player_) :: (disabled <| not <| enabledFor player_) :: activeAttr player_)
                [ text <| "player" ++ Player.toString player_ ]
    in
    div [ class "picker" ]
        [ span [] [ text "Choose a player" ]
        , segment Player.PlayerX
        , segment Player.PlayerO
        ]
