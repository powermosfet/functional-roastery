module View exposing (..)

import Html exposing (Html, node, div, h1, p, ul, li, text, header, aside, nav)
import Html.CssHelpers
import Model exposing (Model, Page(..))
import Message exposing (Msg)
import Css
import Styles.Styles as Styles
import Styles.Classes as Class
import Pages.Customers
import Pages.Orders
import Pages.Varieties
import Pages.Storages


{ id, class, classList } =
    Html.CssHelpers.withNamespace "roastery"


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ Css.compile [ Styles.css ] |> .css |> text ]
        , header [ class [ Class.Header ] ]
            [ div [ class [ Class.Wrapper, Class.HeaderWrapper ] ]
                [ h1 [ class [ Class.HeaderTitle ] ] [ text "The Functional Roastery" ]
                , div [ class [ Class.UserWidget ] ] [ text "Log in" ]
                ]
            ]
        , div [ class [ Class.Wrapper ] ]
            [ menu model
            , pageView model.page model
            ]
        ]


pageView : Page -> (Model -> Html Msg)
pageView p =
    case p of
        Customers ->
            Pages.Customers.view

        Orders ->
            Pages.Orders.view

        Varieties ->
            Pages.Varieties.view

        Storages ->
            Pages.Storages.view


appHeader : Model -> Html msg
appHeader _ =
    div [ class [ Class.Header ] ]
        [ h1 [ class [ Class.HeaderTitle ] ] [ text "The Functional Roastery" ]
        , div [ class [ Class.UserWidget ] ] [ text "Log in" ]
        ]


menu : Model -> Html msg
menu _ =
    nav [ class [ Class.Menu ] ]
        [ ul [ class [ Class.MenuList ] ]
            [ li [ class [ Class.MenuItem ] ] [ text "Home" ]
            , li [ class [ Class.MenuItem, Class.ModSelected ] ] [ text "Customers" ]
            , li [ class [ Class.MenuItem ] ] [ text "Varieties" ]
            , li [ class [ Class.MenuItem ] ] [ text "Storages" ]
            , li [ class [ Class.MenuItem ] ] [ text "Orders" ]
            ]
        ]
