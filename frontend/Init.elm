module Init exposing (..)

import RemoteData exposing (RemoteData(..))
import Model exposing (Model, Page(..))
import Message exposing (Msg)


init : ( Model, Cmd Msg )
init =
    ( { credentials = Nothing
      , page = Customers
      , customers = NotAsked
      }
    , Cmd.none
    )
