module Command exposing (..)

import Http
import Json.Decode exposing (Decoder, map, map2, field, string, list)
import Message exposing (Msg(..))
import Model exposing (Customer)


customerDecoder : Decoder Customer
customerDecoder =
    map2 Customer
        (field "name" string)
        (field "email" string)


getCustomers : Cmd Msg
getCustomers =
    let
        url =
            "customer"

        request =
            Http.get url (list customerDecoder)
    in
        Http.send NewCustomerList request
