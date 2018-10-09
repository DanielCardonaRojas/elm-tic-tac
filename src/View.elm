module View exposing (view)

--import Html.Attributes exposing (..)

import Data.Player as Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import View.Game as Game


view : Model -> Html Msg
view model =
    case model.scene of
        MatchSetup str ->
            [ gameSetup (model.room == Nothing) str |> Just
            , Maybe.map roomInfo model.room
            ]
                |> unwrapping (div [ class "initial-screen" ])
                |> List.singleton
                |> template_

        Rematch ->
            rematch model
                |> template

        GamePlay ->
            [ leftPortion model
            , Maybe.map (Game.render model.game) model.player
                |> List.singleton
                |> unwrapping (div [ class "center" ])
            , rightPortion model
            ]
                |> template_

        PlayerChoose ->
            [ playerPicker model |> Just
            , Maybe.map roomInfo model.room
            ]
                |> unwrapping (div [ class "initial-screen" ])
                |> List.singleton
                |> template_


leftPortion : Model -> Html Msg
leftPortion model =
    [ Maybe.map (\p -> playerScore (Tuple.first model.score) (model.turn /= p) "You" p) model.player ]
        |> unwrapping (div [ class "left" ])


rightPortion : Model -> Html Msg
rightPortion model =
    [ Maybe.map (\p -> playerScore (Tuple.second model.score) (model.turn /= p) "Opponent" p) model.opponent
    , Maybe.map roomInfo model.room
    ]
        |> unwrapping (div [ class "right" ])


roomInfo : String -> Html Msg
roomInfo roomName =
    div [ class "room-connection" ]
        [ text <| "Connected to room: " ++ roomName
        ]


template_ : List (Html Msg) -> Html Msg
template_ html =
    div [ class "elm-tic-tac" ]
        [ span [ class "gametitle" ] [ text "Elm-Tic-Tac" ]
        , div [ class "main" ] html
        , footer
        ]


template : Html Msg -> Html Msg
template html =
    template_ <| List.singleton html


turnIndicator : Player -> Player -> Html Msg
turnIndicator player turn =
    if player == turn then
        span [ class "active" ]
            [ text "Your turn" ]
    else
        span [ class "inactive" ]
            [ text "Waiting for opponent move" ]


footer : Html msg
footer =
    div [ class "footer" ]
        [ span []
            [ text "The "
            , a [ href "https://github.com/DanielCardonaRojas/elm-tic-tac", target "_blank" ] [ text "code" ]
            , text " for this game is open sourced and written in Elm"
            ]
        , span
            []
            [ text "© 2018 Daniel Cardona Rojas" ]
        ]



-- Scenes


gameSetup : Bool -> String -> Html Msg
gameSetup enabled room =
    div [ class "setup" ]
        [ h3 [] [ text "Create a new match or join one" ]
        , input [ onInput (String.trim >> String.toLower >> RoomSetup), placeholder "Enter room name", disabled <| not enabled ] []
        , button [ onClick <| CreateGame room, disabled <| not enabled ] [ text "Create Game" ]
        , button [ onClick <| SelectRoom room, disabled <| not enabled ] [ text "Join" ]
        , p [] [ text "Elm-Tic-Tac is a two player online 3D tic tac toe game" ]
        ]


rematch : Model -> Html Msg
rematch model =
    div [ class "rematch" ] [ text "Rematch" ]


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


playerScore : Int -> Bool -> String -> Player -> Html msg
playerScore score disabled title player =
    div
        (class "score"
            :: playerClass player
            :: (if disabled then
                    [ class "disabled" ]
                else
                    []
               )
        )
        [ span [] [ text <| title ++ ": " ++ String.fromInt score ]
        ]


unwrapping : (List (Html msg) -> Html msg) -> List (Maybe (Html msg)) -> Html msg
unwrapping wrapper ls =
    List.filterMap identity ls
        |> wrapper


unwrapped : String -> List (Maybe (Html msg)) -> Html msg
unwrapped divClass ls =
    unwrapping (div [ class divClass ]) ls
