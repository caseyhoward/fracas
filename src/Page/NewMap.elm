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
import GameMap
import Graphql.Http
import Graphql.Http.GraphqlError
import Html
import Http
import RemoteData
import Session
import Tuple
import ViewHelpers


type alias Model =
    { session : Session.Session
    , rawMap : String
    , name : String
    , mapPreview : GameMap.NewMap
    , savingMap : RemoteData.RemoteData (Graphql.Http.Error GameMap.GameMap) GameMap.GameMap
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , name = ""
      , rawMap = ""
      , savingMap = RemoteData.NotAsked
      , mapPreview = GameMap.parse "" ""
      }
    , Cmd.none
    )


type Msg
    = CreateMap
    | CreatedMap (RemoteData.RemoteData (Graphql.Http.Error GameMap.GameMap) GameMap.GameMap)
    | UpdateRawMap String
    | UpdateName String
    | WindowResized Int Int



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateMap ->
            ( model, GameMap.create model.mapPreview CreatedMap )

        CreatedMap savingMap ->
            ( { model | savingMap = savingMap }, Cmd.none )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateRawMap rawMap ->
            ( { model | rawMap = rawMap, mapPreview = GameMap.parse model.name rawMap }, Cmd.none )

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
                , GameMap.view ViewHelpers.pixelsPerMapSquare model.mapPreview.countries model.mapPreview.dimensions |> Element.html |> Element.el [ Element.width Element.fill ]
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
