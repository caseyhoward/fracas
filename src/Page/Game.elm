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
import Element
import Game
import Graphql.Http
import Html
import Map
import RemoteData
import Session


type alias Model =
    { session : Session.Session
    , game : RemoteData.RemoteData (Graphql.Http.Error Game.Game) Game.Game

    -- , map : RemoteData.RemoteData (Graphql.Http.Error Map.Map) Map.Map
    }


type Msg
    = WindowResized Int Int
    | GotGame (RemoteData.RemoteData (Graphql.Http.Error Game.Game) Game.Game)



-- | GotMap (RemoteData.RemoteData (Graphql.Http.Error Map.Map) Map.Map)


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
    { title = "", content = content model }


content : Model -> Html.Html Msg
content model =
    Element.layout [ Element.width Element.fill ]
        (case model.game of
            RemoteData.Success game ->
                Element.column
                    []
                    [ Map.view game.map.countries game.map.dimensions |> Element.html ]

            _ ->
                Element.text "whatever"
        )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
