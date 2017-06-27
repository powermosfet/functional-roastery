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
    | CustomerList
    | VarietyList
    | OrderList
    | StorageList
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ map Home top
        , map CustomerList (s "customers")
        , map VarietyList (s "varieties")
        , map OrderList (s "orders")
        , map StorageList (s "storages")
        ]


parse : Navigation.Location -> Route
parse loc =
    parseHash route loc
        |> Maybe.withDefault NotFound


toUrl : Route -> String
toUrl route =
    case route of
        CustomerList ->
            "#/customers"

        VarietyList ->
            "#/varieties"

        OrderList ->
            "#/orders"

        StorageList ->
            "#/storages"

        _ ->
            "#"
