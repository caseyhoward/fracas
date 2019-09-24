module Main exposing (main)

-- import Maps.Big
-- import Maps.UnitedStates

import ActiveGame
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Collage
import Collage.Events
import Collage.Layout
import Collage.Render
import Collage.Text
import Color
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import GameMap
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Maps.Big
import Page
import Page.ActiveGame
import Random
import Random.Dict
import Random.List
import Set
import Task
import Time
import TroopCount
import Url
import ViewHelpers



-- Settings


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10



---- MODEL ----


type Model
    = ConfiguringGame ConfigurationAttributes
    | ActiveGame Page.ActiveGame.Model
    | GeneratingRandomTroopCounts ConfigurationAttributes GameMap.GameMap


type alias ConfigurationAttributes =
    { numberOfPlayers : String }


main : Program {} Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> Noop
        , onUrlChange = \_ -> Noop
        }


init : {} -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( ConfiguringGame { numberOfPlayers = "2" }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NumberOfPlayersChanged String
    | StartGameClicked
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | NumberOfPlayersKeyPressed Int
    | WindowResized Int Int
    | Noop
    | GotActiveGameMsg Page.ActiveGame.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( _, ConfiguringGame configurationOptions ) ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { configurationOptions | numberOfPlayers = numberOfPlayers }, Cmd.none )

                StartGameClicked ->
                    startGame configurationOptions

                NumberOfPlayersKeyPressed key ->
                    if key == 13 then
                        startGame configurationOptions

                    else
                        ( model, Cmd.none )

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                WindowResized _ _ ->
                    ( model, Cmd.none )

                Noop ->
                    ( model, Cmd.none )

                GotActiveGameMsg subMsg ->
                    ( model, Cmd.none )

        ( _, GeneratingRandomTroopCounts configurationOptions map ) ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    let
                        numberOfPlayers =
                            configurationOptions.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    ( ActiveGame (ActiveGame.start map numberOfPlayers neutralCountryTroopCounts)
                    , Task.attempt
                        (\viewportResult ->
                            case viewportResult of
                                Ok viewport ->
                                    WindowResized (round viewport.viewport.width) (round viewport.viewport.height)

                                Err _ ->
                                    WindowResized 0 0
                        )
                        Browser.Dom.getViewport
                    )

                _ ->
                    ( model, Cmd.none )

        ( GotActiveGameMsg subMsg, ActiveGame activeGame ) ->
            Page.ActiveGame.update subMsg activeGame
                |> updateWith ActiveGame GotActiveGameMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String TroopCount.TroopCount)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        100
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (TroopCount.random maximumNeutralCountryTroops)


startGame : ConfigurationAttributes -> ( Model, Cmd Msg )
startGame configurationOptions =
    let
        map =
            GameMap.parse Maps.Big.map ActiveGame.pixelsPerMapSquare
    in
    ( GeneratingRandomTroopCounts configurationOptions map
    , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys map.countries))
    )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        -- viewPage : Page.Page -> (a -> Msg) -> { title : String, content : Html.Html Msg } -> Browser.Document Msg
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        ConfiguringGame configuringGameSettings ->
            { title = "", body = [ viewGameConfiguration configuringGameSettings ] }

        GeneratingRandomTroopCounts _ _ ->
            { title = "", body = [ Element.layout [] Element.none ] }

        ActiveGame activeGame ->
            viewPage Page.ActiveGame GotActiveGameMsg (Page.ActiveGame.view activeGame)



-- viewPlayingGame activeGame
---- Configuration


viewGameConfiguration : ConfigurationAttributes -> Html.Html Msg
viewGameConfiguration gameConfiguration =
    Element.layout [ Element.width Element.fill, Element.centerX ]
        (Element.column
            [ Element.width Element.fill, Element.centerX ]
            [ Element.el
                [ Element.padding 100
                , Element.Font.bold
                , Element.Font.size 80
                , Element.centerX
                , Element.Font.color (Color.darkBlue |> ViewHelpers.colorToElementColor)
                ]
                (Element.text "Fracas")
            , Element.el [ Element.centerX ]
                (Element.column
                    [ Element.width Element.fill ]
                    [ Element.Input.text
                        [ Element.width (Element.px 50)
                        , Element.htmlAttribute
                            (Html.Events.on
                                "keyup"
                                (Json.map NumberOfPlayersKeyPressed Html.Events.keyCode)
                            )
                        ]
                        { onChange = NumberOfPlayersChanged
                        , text = gameConfiguration.numberOfPlayers
                        , placeholder = Nothing
                        , label =
                            Element.Input.labelLeft
                                [ Element.centerY
                                , Element.paddingEach { top = 0, left = 0, right = 10, bottom = 0 }
                                ]
                                (Element.text "Number of players")
                        }
                    , Element.Input.button
                        (ViewHelpers.defaultButtonAttributes
                            ++ [ Element.Background.color (Element.rgb255 0 150 0)
                               , Element.width Element.fill
                               , Element.padding 20
                               , Element.centerX
                               , Element.moveDown 30
                               , Element.Font.size 30
                               , Element.Font.color (Color.white |> ViewHelpers.colorToElementColor)
                               ]
                        )
                        { onPress = Just StartGameClicked, label = ViewHelpers.centerText "Start Game" }
                    ]
                )
            ]
        )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize (\x y -> WindowResized x y)
