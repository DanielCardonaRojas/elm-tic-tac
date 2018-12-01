module Model exposing (Model, Scene(..), default, device, styler)

import Data.Game as Game exposing (Game)
import Data.Player as Player exposing (Player)
import Element exposing (Device)
import Msg exposing (Msg(..))
import Style.Process as Style exposing (Styler)
import Style.Rules as Style exposing (Rules)


type Scene
    = MatchSetup String
    | PlayerChoose
    | GamePlay
    | Rematch


type alias Model =
    { game : Game
    , player : Maybe Player -- The local player, assigned be websocket server.
    , opponent : Maybe Player
    , socketId : Maybe String
    , room : Maybe String
    , score : ( Int, Int ) -- Player and opponent score
    , scene : Scene
    , windowSize : ( Int, Int )
    }


device : Model -> Device
device model =
    Element.classifyDevice
        { width = model.windowSize |> Tuple.first
        , height = model.windowSize |> Tuple.second
        }


styler : Model -> Styler Rules Msg
styler model =
    device model
        |> Debug.log "Device"
        |> Just
        |> Style.styled


default : Model
default =
    { game = Game.make 3
    , player = Nothing
    , opponent = Nothing
    , socketId = Nothing
    , room = Nothing
    , score = ( 0, 0 )
    , scene = MatchSetup ""
    , windowSize = ( 0, 0 )
    }
