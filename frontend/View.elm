module View exposing (..)

import Html exposing (Html, node, div, h1, p, ul, li, text)
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
        , div [ class [ Class.Wrapper ] ]
            [ header model
            , menu model
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


header : Model -> Html msg
header _ =
    div [ class [ Class.Header ] ]
        [ h1 [ class [ Class.HeaderTitle ] ] [ text "The Functional Roastery" ]
        , div [ class [ Class.UserWidget ] ] [ text "Log in" ]
        ]


menu : Model -> Html msg
menu _ =
    div [] []
