module Api exposing (..)


type ApiUrl
    = Me
    | CustomerList


getUrl : ApiUrl -> String
getUrl u =
    case u of
        Me ->
            "/user/me"

        CustomerList ->
            "/customer/"
