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
import Element.Input
import Html
import Map
import Session
import ViewHelpers


type alias Model =
    { session : Session.Session
    , rawMap : String
    , name : String
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , name = ""
      , rawMap = ""
      }
    , Cmd.none
    )


type Msg
    = UpdateRawMap String
    | UpdateName String
    | WindowResized Int Int



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateRawMap rawMap ->
            ( { model | rawMap = rawMap }, Cmd.none )

        WindowResized width height ->
            ( { model | session = model.session |> Session.updateWindowSize { width = width, height = height } }, Cmd.none )



---- VIEW ----


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = "New map"
    , content =
        Element.layout [ Element.width Element.fill ]
            (Element.column
                [ Element.centerX, Element.width (Element.px 1000), Element.spacing 20, Element.padding 20 ]
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
                ]
            )
    }


toSession : Model -> Session.Session
toSession model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
