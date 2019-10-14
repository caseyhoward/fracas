module Page.JoinInternetGame exposing
    ( Model
    , Msg
    , init
    , toSession
    , update
    , view
    )

import Element
import Graphql.Http
import Html
import InternetGame
import RemoteData
import Route
import Session


type alias Model =
    { session : Session.Session
    , joinedGameRemoteData : RemoteData.RemoteData (Graphql.Http.Error InternetGame.PlayerToken) InternetGame.PlayerToken
    }


type Msg
    = JoinedGame (RemoteData.RemoteData (Graphql.Http.Error InternetGame.PlayerToken) InternetGame.PlayerToken)


init : Session.Session -> InternetGame.JoinToken -> ( Model, Cmd Msg )
init session playerToken =
    ( { session = session, joinedGameRemoteData = RemoteData.Loading }
    , InternetGame.joinGame session.apiUrl playerToken JoinedGame
    )


view : Model -> { title : String, content : Html.Html Msg }
view _ =
    { title = "Joining game", content = Element.layout [] (Element.text "Joining...") }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        JoinedGame playerTokenRemoteData ->
            case playerTokenRemoteData of
                RemoteData.Success playerToken ->
                    ( { model | joinedGameRemoteData = playerTokenRemoteData }, Route.pushUrl model.session.navKey (Route.InternetGame playerToken) )

                _ ->
                    ( model, Cmd.none )


toSession : Model -> Session.Session
toSession model =
    model.session
