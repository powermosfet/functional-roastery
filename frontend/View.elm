module View exposing (..)

import Css
import Html
    exposing
        ( Html
        , node
        , div
        , h1
        , p
        , ul
        , li
        , text
        , header
        , a
        , aside
        , nav
        )
import Html.Attributes exposing (href)
import Html.CssHelpers
import Message exposing (Msg)
import Model exposing (Model)
import Pages.Home
import Pages.Login
import Pages.Customers
import Pages.Orders
import Pages.Storages
import Pages.Varieties
import Pages.NotFound
import Route
import Styles.Classes as Class
import Styles.Styles as Styles


{ id, class, classList } =
    Html.CssHelpers.withNamespace "roastery"


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ Css.compile [ Styles.css ] |> .css |> text ]
        , header [ class [ Class.Header ] ]
            [ div [ class [ Class.Wrapper, Class.HeaderWrapper ] ]
                [ h1 [ class [ Class.HeaderTitle ] ] [ text "The Functional Roastery" ]
                , userWidget model
                ]
            ]
        , div [ class [ Class.Wrapper ] ]
            [ menu model
            , pageView model.route model
            ]
        ]


pageView : Route.Route -> (Model -> Html Msg)
pageView r =
    case r of
        Route.Home ->
            Pages.Home.view

        Route.Login ->
            Pages.Login.view

        Route.CustomerList ->
            Pages.Customers.view

        Route.VarietyList ->
            Pages.Varieties.view

        Route.OrderList ->
            Pages.Orders.view

        Route.StorageList ->
            Pages.Storages.view

        Route.NotFound ->
            Pages.NotFound.view


appHeader : Model -> Html msg
appHeader model =
    div [ class [ Class.Header ] ]
        [ h1 [ class [ Class.HeaderTitle ] ] [ text "The Functional Roastery" ]
        , userWidget model
        ]


userWidget : Model -> Html msg
userWidget model =
    case model.session of
        Just { profile, credentials } ->
            div [] []

        Nothing ->
            div [ class [ Class.UserWidget ] ]
                [ a
                    [ href (Route.toUrl Route.Login)
                    , class [ Class.DiscreteLink ]
                    ]
                    [ text "Log in" ]
                ]


menu : Model -> Html msg
menu model =
    let
        itemLabel route =
            case route of
                Route.Home ->
                    "Home"

                Route.CustomerList ->
                    "Customers"

                Route.VarietyList ->
                    "Varieties"

                Route.OrderList ->
                    "Orders"

                Route.StorageList ->
                    "Storages"

                _ ->
                    ""

        selected route =
            route == model.route

        classes route =
            if selected route then
                [ Class.MenuItem, Class.ModSelected ]
            else
                [ Class.MenuItem ]

        item route =
            a [ class [ Class.DiscreteLink ], href (Route.toUrl route) ] [ li [ class (classes route) ] [ text (itemLabel route) ] ]
    in
        nav [ class [ Class.Menu ] ]
            [ ul [ class [ Class.MenuList ] ]
                [ item Route.Home
                , item Route.CustomerList
                , item Route.VarietyList
                , item Route.OrderList
                , item Route.StorageList
                ]
            ]
