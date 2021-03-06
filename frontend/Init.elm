module Init exposing (..)

import Message exposing (Msg)
import Model exposing (Model, Credentials)
import Navigation
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))


init : Navigation.Location -> ( Model, Cmd Msg )
init url =
    ( { session = Nothing
      , route = Route.parse url
      , customers = NotAsked
      , loginForm = Credentials "" ""
      }
    , Cmd.none
    )
