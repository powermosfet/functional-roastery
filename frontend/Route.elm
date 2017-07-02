module Route exposing (..)

import Navigation
import UrlParser
    exposing
        ( (</>)
        , Parser
        , s
        , int
        , map
        , oneOf
        , top
        , parseHash
        )


type Route
    = Home
    | Login
    | CustomerList
    | VarietyList
    | OrderList
    | StorageList
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map CustomerList (s "customers")
        , map VarietyList (s "varieties")
        , map OrderList (s "orders")
        , map StorageList (s "storages")
        ]


parse : Navigation.Location -> Route
parse loc =
    parseHash route loc
        |> Maybe.withDefault NotFound


urlify : List String -> String
urlify parts =
    "#/" ++ (String.join "/" parts)


toUrl : Route -> String
toUrl route =
    case route of
        Login ->
            urlify [ "login" ]

        CustomerList ->
            urlify [ "customers" ]

        VarietyList ->
            urlify [ "varieties" ]

        OrderList ->
            urlify [ "orders" ]

        StorageList ->
            urlify [ "storages" ]

        _ ->
            urlify []
