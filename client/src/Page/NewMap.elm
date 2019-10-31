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
import Element
import Element.Background
import Element.Input
import Graphql.Http
import Html
import Map
import RemoteData
import Session
import UserMap
import ViewHelpers


type alias Model =
    { session : Session.Session
    , rawMap : String
    , newMap : Map.Map
    , savingMap : RemoteData.RemoteData (Graphql.Http.Error UserMap.UserMap) UserMap.UserMap
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , rawMap = ""
      , savingMap = RemoteData.NotAsked
      , newMap = Map.parse "" ""
      }
    , Cmd.none
    )


type Msg
    = CreateMap
    | CreatedMap (RemoteData.RemoteData (Graphql.Http.Error UserMap.UserMap) UserMap.UserMap)
    | UpdateRawMap String
    | UpdateName String
    | WindowResized Int Int



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateMap ->
            ( model, UserMap.create model.session.apiUrl model.newMap CreatedMap )

        CreatedMap savingMap ->
            ( { model | savingMap = savingMap }, Cmd.none )

        UpdateName name ->
            ( { model | newMap = model.newMap |> Map.newMapWithName name }, Cmd.none )

        UpdateRawMap rawMap ->
            ( { model | rawMap = rawMap, newMap = Map.parse model.newMap.name rawMap }, Cmd.none )

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
                    , text = model.newMap.name
                    }
                , Element.Input.multiline
                    (ViewHelpers.defaultTextInputAttributes ++ [ Element.height (Element.px 500) ])
                    { onChange = UpdateRawMap
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove (ViewHelpers.defaultLabelAttributes ++ [ Element.alignLeft ]) (Element.text "Map file text")
                    , text = model.rawMap
                    , spellcheck = False
                    }
                , Map.view ViewHelpers.pixelsPerMapSquare model.newMap.countries model.newMap.dimensions |> Element.html |> Element.el [ Element.width Element.fill ]
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
                            Element.text ("Error: " ++ (err |> ViewHelpers.errorToString))

                        RemoteData.Success _ ->
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
