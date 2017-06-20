module Pages.Customers exposing (..)

import Html exposing (Html)
import Model exposing (Model)
import Message exposing (Msg)


view : Model -> Html Msg
view _ =
    Html.div []
        [ Html.h2 [] [ Html.text "Cusomters" ]
        ]
