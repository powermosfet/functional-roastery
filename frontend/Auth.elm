module Auth exposing (..)

import Http exposing (empty)
import Json.Decode exposing (Decoder, map, map2, field, string, list)
import Message exposing (Msg(..))
import Model exposing (Cat, Customer)


type Auth
    = Basic { username : String, password : String }


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


makeRequest : Auth -> Verb -> String -> Task RawError Response
makeRequest auth verb url =
    let
        headers =
            case auth of
                Basic username password ->
                    [ ( "Authorization", "Basic " ++ buildAuthorizationToken username password ) ]
    in
        defaultSettings
            { verb = verbString verb
            , headers = headers
            , url = url
            , body = empty
            }
