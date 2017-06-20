module Main exposing (main)

import Html
import Init
import Model
import Message
import Update
import View


main : Program Never Model.Model Message.Msg
main =
    Html.program
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        }
