module Message exposing (..)

import Http
import Model exposing (Customer, UserProfile)
import Navigation


type Msg
    = DoLogin
    | NewCustomerList (Result Http.Error (List Customer))
    | NewUrl Navigation.Location
    | EnterUsername String
    | EnterPassword String
    | LoginComplete (Result Http.Error UserProfile)
