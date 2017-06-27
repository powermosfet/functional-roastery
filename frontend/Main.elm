module Main exposing (main)

import Init
import Model
import Message
import Update
import View
import Navigation


main =
    Navigation.program Message.NewUrl
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        }
