module Message exposing (..)

import Http
import Model exposing (Customer)
import Navigation


type Msg
    = DoLogin
    | NewCustomerList (Result Http.Error (List Customer))
    | NewUrl Navigation.Location
    | EnterUsername String
    | EnterPassword String
