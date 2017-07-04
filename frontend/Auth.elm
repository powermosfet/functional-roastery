module Auth exposing (..)

import Http exposing (Request, request, emptyBody, expectJson)
import BasicAuth exposing (buildAuthorizationHeader)
import Json.Decode exposing (Decoder, map, map2, field, string, list)
import Message exposing (Msg(..))
import Model exposing (Customer, Credentials)


type Verb
    = Get
    | Post


verbString : Verb -> String
verbString verb =
    case verb of
        Get ->
            "GET"

        Post ->
            "POST"


makeRequest : Credentials -> Verb -> String -> Decoder a -> Request a
makeRequest { username, password } verb url decoder =
    let
        headers =
            [ buildAuthorizationHeader username password ]
    in
        request
            { method = verbString verb
            , headers = headers
            , url = url
            , body = emptyBody
            , expect = expectJson decoder
            , timeout = Nothing
            , withCredentials = True
            }


authGet : Credentials -> String -> Decoder a -> Request a
authGet credentials url decoder =
    makeRequest credentials Get url decoder
