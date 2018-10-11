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
import View.Setup as Setup


view : Model -> Html Msg
view model =
    case model.scene of
        MatchSetup str ->
            [ Setup.connection (model.room == Nothing) str |> Just
            , Maybe.map roomInfo model.room
            ]
                |> unwrapping (div [ class "initial-screen" ])
                |> List.singleton
                |> template_

        PlayerChoose ->
            [ Setup.playerPicker model |> Just
            , Maybe.map roomInfo model.room
            ]
                |> unwrapping (div [ class "initial-screen" ])
                |> List.singleton
                |> template_

        GamePlay ->
            [ leftPortion model
            , Maybe.map (Game.render model.game) model.player
                |> List.singleton
                |> unwrapping (div [ class "center" ])
            , rightPortion model
            ]
                |> template_

        Rematch ->
            rematch model
                |> template


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


playerClass : Player -> Attribute msg
playerClass p =
    "player" ++ Player.toString p |> String.toLower |> class



-- Scenes


rematch : Model -> Html Msg
rematch model =
    div [ class "rematch" ] [ text "Rematch" ]


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
