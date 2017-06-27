module Update exposing (..)

import RemoteData
import Message exposing (Msg(..))
import Model exposing (Model)
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
    case message of
        NewCustomerList result ->
            ( { model | customers = resultToRemoteData result }, Cmd.none )

        NewUrl url ->
            ( { model | route = Route.parse url }, Cmd.none )
