module Page.EditMap exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , view
    )

import Browser.Events
import Html
import Map
import Session


type alias Model =
    { session : Session.Session }


init : Session.Session -> Map.Id -> ( Model, Cmd Msg )
init session _ =
    ( { session = session }
    , Cmd.none
    )


type Msg
    = RawMapChanged String
    | WindowResized Int Int


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = "Editing map"
    , content = Html.div [] []
    }


toSession : Model -> Session.Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
