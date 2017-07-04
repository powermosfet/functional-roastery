module Update exposing (..)

import RemoteData
import Message exposing (Msg(..))
import Model exposing (Model)
import Command exposing (getUserProfile)
import Route
import UrlParser exposing (parseHash)


resultToRemoteData : Result a b -> RemoteData.RemoteData a b
resultToRemoteData result =
    case result of
        Ok data ->
            RemoteData.Success data

        Err error ->
            RemoteData.Failure error


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        n m =
            ( m, Cmd.none )

        loginForm =
            model.loginForm

        session =
            model.session
    in
        case message of
            NewCustomerList result ->
                n { model | customers = resultToRemoteData result }

            NewUrl url ->
                n { model | route = Route.parse url }

            EnterUsername username ->
                n { model | loginForm = { loginForm | username = username } }

            EnterPassword password ->
                n { model | loginForm = { loginForm | password = password } }

            DoLogin ->
                ( model, getUserProfile model.loginForm )

            LoginComplete result ->
                case result of
                    Ok profile ->
                        n
                            { model
                                | session = Just { credentials = model.loginForm, profile = profile }
                                , loginForm = { username = "", password = "" }
                            }

                    _ ->
                        n model
