module Model exposing (..)

import RemoteData exposing (RemoteData)
import Http


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


type Page
    = Customers
    | Orders
    | Varieties
    | Storages


type alias Model =
    { credentials : Maybe Credentials
    , page : Page
    , customers : WebData (List Customer)
    }
