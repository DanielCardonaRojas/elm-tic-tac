module Model exposing (..)

import Data.Game as Game exposing (Game)
import Data.Player as Player exposing (Player)
import Element
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


styler : Model -> Styler Rules Msg
styler model =
    Element.classifyDevice
        { height = model.windowSize |> Tuple.first
        , width = model.windowSize |> Tuple.second
        }
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
