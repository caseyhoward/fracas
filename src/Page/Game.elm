module Page.Game exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Browser.Events
import Game
import Graphql.Http
import Html
import RemoteData
import Session


type alias Model =
    { session : Session.Session
    , game : RemoteData.RemoteData (Graphql.Http.Error Game.Game) Game.Game
    }


type Msg
    = WindowResized Int Int
    | GotGame (RemoteData.RemoteData (Graphql.Http.Error Game.Game) Game.Game)


init : Session.Session -> Game.Id -> ( Model, Cmd Msg )
init session gameId =
    ( { session = session
      , game = RemoteData.NotAsked
      }
    , Game.get gameId GotGame
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGame game ->
            ( { model | game = game }, Cmd.none )

        WindowResized width height ->
            ( { model | session = Session.updateWindowSize { width = width, height = height } model.session }, Cmd.none )


toSession : Model -> Session.Session
toSession model =
    model.session



---- VIEW ----


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = "", content = Html.div [] [] }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
