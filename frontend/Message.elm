module Message exposing (..)

import Http
import Model exposing (Customer)


type Msg
    = NewCustomerList (Result Http.Error (List Customer))
