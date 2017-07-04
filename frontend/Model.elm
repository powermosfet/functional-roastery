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


type alias UserProfile =
    { username : String
    , id : Int
    }


type alias Customer =
    { name : String
    , email : String
    }


type alias Model =
    { session :
        Maybe
            { credentials : Credentials
            , profile : UserProfile
            }
    , route : Route
    , customers : WebData (List Customer)
    , loginForm : Credentials
    }
