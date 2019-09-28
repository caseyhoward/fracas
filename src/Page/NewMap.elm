module Page.NewMap exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Browser.Events
import Collage
import Color
import Element
import Element.Background
import Element.Input
import Graphql.Http
import Graphql.Http.GraphqlError
import Html
import Http
import Map
import RemoteData
import Session
import Tuple
import ViewHelpers


type alias Model =
    { session : Session.Session
    , rawMap : String
    , name : String
    , mapPreview : Map.NewMap
    , savingMap : RemoteData.RemoteData (Graphql.Http.Error Map.Map) Map.Map
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , name = ""
      , rawMap = ""
      , savingMap = RemoteData.NotAsked
      , mapPreview = Map.parse "" ""
      }
    , Cmd.none
    )


type Msg
    = CreateMap
    | CreatedMap (RemoteData.RemoteData (Graphql.Http.Error Map.Map) Map.Map)
    | UpdateRawMap String
    | UpdateName String
    | WindowResized Int Int



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateMap ->
            let
                newMap =
                    Map.parse model.name model.rawMap
            in
            ( model, Map.create newMap CreatedMap )

        CreatedMap savingMap ->
            ( { model | savingMap = savingMap }, Cmd.none )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateRawMap rawMap ->
            ( { model | rawMap = rawMap, mapPreview = Map.parse model.name rawMap }, Cmd.none )

        WindowResized width height ->
            ( { model | session = model.session |> Session.updateWindowSize { width = width, height = height } }, Cmd.none )



---- VIEW ----


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = "New map"
    , content =
        Element.layout [ Element.width Element.fill ]
            (Element.column
                [ Element.centerX
                , Element.width (Element.fill |> Element.maximum 1000)
                , Element.spacing 20
                , Element.padding 20
                ]
                [ Element.el [] (Element.text "Create a new map")
                , Element.Input.text
                    ViewHelpers.defaultTextInputAttributes
                    { onChange = UpdateName
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove (ViewHelpers.defaultLabelAttributes ++ [ Element.alignLeft ]) (Element.text "Map name")
                    , text = model.name
                    }
                , Element.Input.multiline
                    (ViewHelpers.defaultTextInputAttributes ++ [ Element.height (Element.px 500) ])
                    { onChange = UpdateRawMap
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove (ViewHelpers.defaultLabelAttributes ++ [ Element.alignLeft ]) (Element.text "Map file text")
                    , text = model.rawMap
                    , spellcheck = False
                    }
                , Map.view model.mapPreview |> Element.html |> Element.el [ Element.width Element.fill ]
                , Element.Input.button
                    (ViewHelpers.defaultButtonAttributes
                        ++ [ Element.width (Element.px 120)
                           , Element.centerX
                           , 40 |> Element.px |> Element.height
                           , Element.Background.color (Element.rgb255 100 200 100)
                           ]
                    )
                    { onPress = Just CreateMap, label = ViewHelpers.centerText "Create Map" }
                , Element.el []
                    (case model.savingMap of
                        RemoteData.NotAsked ->
                            Element.text "not asked"

                        RemoteData.Loading ->
                            Element.text "Loading."

                        RemoteData.Failure err ->
                            Element.text ("Error: " ++ (err |> errorToString))

                        RemoteData.Success news ->
                            Element.text "Redirecting"
                    )
                ]
            )
    }


toSession : Model -> Session.Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError httpError ->
            case httpError of
                Graphql.Http.BadUrl url ->
                    "Http error: Bad url - " ++ url

                Graphql.Http.Timeout ->
                    "Http error: timeout"

                Graphql.Http.NetworkError ->
                    "Http error: network error"

                Graphql.Http.BadStatus metadata string ->
                    "Http error: bad status - " ++ string

                Graphql.Http.BadPayload error ->
                    "Http error: bad payload"


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message
