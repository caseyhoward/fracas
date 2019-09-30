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
import Html
import Session


type alias Model =
    { session : Session.Session }


type Msg
    = WindowResized Int Int


init : Session.Session -> Game.Id -> ( Model, Cmd Msg )
init session gameId =
    ( { session = session }, Cmd.none )


toSession : Model -> Session.Session
toSession model =
    model.session


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = "", content = Html.div [] [] }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
