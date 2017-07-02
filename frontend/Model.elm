module Model exposing (..)

import RemoteData exposing (RemoteData)
import Http
import Route exposing (Route)


type alias WebData a =
    RemoteData Http.Error a


type alias Credentials =
    { username : String
    , password : String
    }


type alias Customer =
    { name : String
    , email : String
    }


type alias Model =
    { credentials : Maybe Credentials
    , route : Route
    , customers : WebData (List Customer)
    , loginForm : Credentials
    }
