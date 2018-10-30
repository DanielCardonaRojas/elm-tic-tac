module View.Setup exposing (connection, playerPicker)

import Constants as Const
import Data.Player as Player exposing (Player(..))
import Element exposing (Element, el, fill, height, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Maybe.Extra as Maybe
import Model exposing (..)
import Msg exposing (Msg(..))
import Style.Rules as Style exposing (style)


maybeIf : Bool -> a -> Maybe a
maybeIf b v =
    if b then
        Just v
    else
        Nothing


connection : Bool -> String -> Element Msg
connection enabled room =
    let
        button txt msg =
            Input.button
                (Element.centerX :: width fill :: style Style.SetupButton)
                { label = el [ Element.centerX ] <| text txt
                , onPress = maybeIf enabled msg
                }
    in
    Element.column (Element.centerX :: style Style.Setup)
        [ Input.text []
            { onChange = String.trim >> String.toLower >> RoomSetup
            , placeholder = maybeIf enabled (Input.placeholder [] <| text "Enter room name")
            , label = Input.labelAbove [ Font.size Const.ui.fontSize.medium ] <| text "Create a new match or join one"
            , text = room
            }
        , button "Create Game" <| CreateGame room
        , button "Join" <| SelectRoom room
        , el [ Element.centerX, Font.size Const.ui.fontSize.small ] <| text "Elm-Tic-Tac is a two player online 3D tic tac toe game"
        ]


playerPicker : Model -> Element Msg
playerPicker model =
    let
        isCurrentPlayer player =
            Maybe.map (\p -> p == player) model.player |> Maybe.withDefault False

        enabledFor player =
            Maybe.map (\p -> p /= player) model.opponent
                |> Maybe.withDefault True

        segment player =
            Input.button
                (Element.centerX :: (style <| Style.PlayerButton player))
                { label = text <| "Player" ++ Player.toString player
                , onPress = maybeIf (enabledFor player) (SetPlayer player)
                }
    in
    Element.column (width (Element.fill |> Element.minimum 300) :: style Style.Setup)
        [ el [ Element.centerX ] <| text "Choose a player"
        , segment Player.PlayerX
        , segment Player.PlayerO
        ]
