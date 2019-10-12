module Page.InternetGame exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Api.Mutation
import Game
import Graphql.Http
import Graphql.SelectionSet
import Html
import InternetGame
import RemoteData
import Session


type Msg
    = GotGame (RemoteData.RemoteData (Graphql.Http.Error InternetGame.GameOrConfiguration) InternetGame.GameOrConfiguration)


type Model
    = Loading LoadingModel
    | Configuring ConfiguringModel
    | Playing PlayingModel


type alias LoadingModel =
    { session : Session.Session
    , game : RemoteData.RemoteData (Graphql.Http.Error InternetGame.GameOrConfiguration) InternetGame.GameOrConfiguration
    }


type alias ConfiguringModel =
    { session : Session.Session, configuration : InternetGame.Configuration }


type alias PlayingModel =
    { session : Session.Session, game : Game.Game }



-- type alias Model =
--     { session : Session.Session
--        }


init : Session.Session -> InternetGame.PlayerToken -> ( Model, Cmd Msg )
init session playerToken =
    ( Loading { session = session, game = RemoteData.Loading }, InternetGame.get playerToken GotGame )


toSession : Model -> Session.Session
toSession model =
    Debug.todo ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading loadingModel ->
            case msg of
                GotGame gameRemoteData ->
                    x gameRemoteData loadingModel.session

        Configuring configuringModel ->
            Debug.todo ""

        Playing playingModel ->
            Debug.todo ""


x : RemoteData.RemoteData (Graphql.Http.Error InternetGame.GameOrConfiguration) InternetGame.GameOrConfiguration -> Session.Session -> ( Model, Cmd Msg )
x gameRemoteData session =
    case gameRemoteData of
        RemoteData.Success (InternetGame.InternetGameConfiguration configuration) ->
            ( Configuring { session = session, configuration = configuration }, Cmd.none )

        RemoteData.Success (InternetGame.InternetGame game) ->
            ( Playing { session = session, game = game }, Cmd.none )

        _ ->
            ( Loading { session = session, game = gameRemoteData }, Cmd.none )


view : Model -> { title : String, content : Html.Html Msg }
view model =
    Debug.todo ""


subscriptions model =
    Sub.none
