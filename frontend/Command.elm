module Command exposing (..)

import Http
import Json.Decode exposing (Decoder, map, map2, field, string, int, list)
import Message exposing (Msg(..))
import Model exposing (Customer, Credentials, UserProfile)
import Auth exposing (authGet)
import Api


customerDecoder : Decoder Customer
customerDecoder =
    map2 Customer
        (field "name" string)
        (field "email" string)


userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map2 UserProfile
        (field "username" string)
        (field "id" int)


getCustomers : Cmd Msg
getCustomers =
    let
        url =
            "customer"

        request =
            Http.get (Api.getUrl Api.CustomerList) (list customerDecoder)
    in
        Http.send NewCustomerList request


getUserProfile : Credentials -> Cmd Msg
getUserProfile credentials =
    let
        request =
            authGet credentials (Api.getUrl Api.Me) userProfileDecoder
    in
        Http.send LoginComplete request
