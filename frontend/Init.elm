module Init exposing (..)

import Message exposing (Msg)
import Model exposing (Model)
import Navigation
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))


init : Navigation.Location -> ( Model, Cmd Msg )
init url =
    ( { credentials = Nothing
      , route = Route.parse url
      , customers = NotAsked
      }
    , Cmd.none
    )
