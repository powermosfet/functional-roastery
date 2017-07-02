module Pages.Login exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Model exposing (Model)
import Message exposing (Msg(..))
import Html.CssHelpers
import Styles.Classes as Class
import Styles.Styles exposing (namespace)


{ id, class, classList } =
    Html.CssHelpers.withNamespace namespace


view : Model -> Html Msg
view _ =
    Html.section [ class [ Class.Page ] ]
        [ Html.h2 [ class [ Class.PageTitle ] ] [ Html.text "Log in" ]
        , Html.form [ Event.onSubmit DoLogin ]
            [ Html.fieldset [ class [ Class.FieldSet ] ]
                [ Html.input [ class [ Class.InputText ], Attr.type_ "text", Event.onInput EnterUsername ] []
                , Html.input [ class [ Class.InputText ], Attr.type_ "password", Event.onInput EnterPassword ] []
                , Html.input [ class [ Class.InputSubmit ], Attr.type_ "submit" ] [ Html.text "Log in" ]
                ]
            ]
        ]
