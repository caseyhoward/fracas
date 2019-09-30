module Page.Game exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
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
      -- | GotGame Game.Game
    | GotGame (RemoteData.RemoteData (Graphql.Http.Error Game.Game) Game.Game)


init : Session.Session -> Game.Id -> ( Model, Cmd Msg )
init session gameId =
    Debug.todo ""



-- ( { session = session, game = RemoteData.NotAsked }, Game.get gameId GotGame )


toSession : Model -> Session.Session
toSession model =
    model.session


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = "", content = Html.div [] [] }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
