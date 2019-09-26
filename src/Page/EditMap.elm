module Page.EditMap exposing
    ( Model
    , Msg
    , subscriptions
    , toSession
    , view
    )

import Browser.Events
import Html
import Session


type alias Model =
    { session : Session.Session }


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
