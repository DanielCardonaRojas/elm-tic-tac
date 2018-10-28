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


maybeIf : Bool -> a -> Maybe a
maybeIf b v =
    if b then
        Just v
    else
        Nothing


paneAttributes =
    [ Background.color Const.ui.themeColor.paneBackground
    , Element.spacing Const.ui.spacing.small
    , Element.paddingXY Const.ui.spacing.small Const.ui.spacing.normal
    , Font.color Const.colors.gray
    , Border.rounded 10
    , Element.centerX
    ]


connection : Bool -> String -> Element Msg
connection enabled room =
    let
        button txt msg =
            Input.button
                [ Element.centerX
                , Element.padding Const.ui.spacing.small
                , width fill
                , Background.color <| Const.ui.themeColor.paneButtonBackground
                ]
                { label = el [ Element.centerX ] <| text txt
                , onPress = maybeIf enabled msg
                }
    in
    Element.column paneAttributes
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
            let
                playerColor =
                    if player == PlayerX then
                        Const.colors.red
                    else
                        Const.colors.blue
            in
            Input.button
                [ Element.centerX
                , Element.padding Const.ui.spacing.small
                , Background.color <| playerColor
                ]
                { label = text <| "Player" ++ Player.toString player
                , onPress = maybeIf (enabledFor player) (SetPlayer player)
                }
    in
    Element.column (width (Element.fill |> Element.minimum 300) :: paneAttributes)
        [ el [ Element.centerX ] <| text "Choose a player"
        , segment Player.PlayerX
        , segment Player.PlayerO
        ]


class =
    Element.htmlAttribute << Html.Attributes.class
