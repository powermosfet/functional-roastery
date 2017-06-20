module View exposing (..)

import Html exposing (Html, node, div, h1, p, ul, li, text, header)
import Html.CssHelpers
import Model exposing (Model, Page(..))
import Message exposing (Msg)
import Css
import Styles.Styles as Styles
import Styles.Classes as Class
import Pages.Customers


{ id, class, classList } =
    Html.CssHelpers.withNamespace "roastery"


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ Css.compile [ Styles.css ] |> .css |> text ]
        , div [ class [ Class.App ] ]
            [ header []
                [ div [ class [ Class.Title ] ]
                    [ h1 [] [ text "The Functinal Roastery" ]
                    ]
                , div [ class [ Class.UserWidget ] ]
                    [ text "Log in"
                    ]
                ]
            , div [ class [ Class.Main ] ] []
            ]
        ]


pageView : Page -> (Model -> Html Msg)
pageView p =
    case p of
        Customers ->
            Pages.Customers.view

        _ ->
            \_ -> div [] []
