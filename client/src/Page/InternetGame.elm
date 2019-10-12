module Page.InternetGame exposing (Model, Msg, init, subscriptions, toSession, view)

-- import Api.Object
-- import Api.Object.NewInternetGameTokens

import Api.Mutation
import Graphql.Http
import Graphql.SelectionSet
import Html
import InternetGame
import RemoteData
import Session


type Msg
    = Noop


type alias Model =
    { session : Session.Session }


init : Session.Session -> InternetGame.PlayerToken -> ( Model, Cmd Msg )
init session playerToken =
    Debug.todo ""


toSession : Model -> Session.Session
toSession model =
    model.session


view : Model -> { title : String, content : Html.Html Msg }
view model =
    Debug.todo ""


subscriptions model =
    Sub.none
