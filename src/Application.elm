module Application exposing (main)

import Html
import Model exposing (..)
import Msg exposing (Msg)
import Ports.Echo as Echo
import Ports.LocalStorage as LocalStorage
import Ports.SocketIO as SocketIO
import View


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = View.view
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


init : ( Model, Cmd Msg )
init =
    {} ! []
