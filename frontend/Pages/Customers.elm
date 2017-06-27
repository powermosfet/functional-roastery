module Pages.Customers exposing (..)

import Html exposing (Html)
import Model exposing (Model)
import Message exposing (Msg)
import Html.CssHelpers
import Styles.Classes as Class
import Styles.Styles exposing (namespace)


{ id, class, classList } =
    Html.CssHelpers.withNamespace namespace


view : Model -> Html Msg
view _ =
    Html.section [ class [ Class.Page ] ]
        [ Html.h2 [ class [ Class.PageTitle ] ] [ Html.text "Customers" ]
        ]
