module Page.GameConfiguration exposing (Model, Msg, init, subscriptions, toSession, update, view)

import ActiveGame
import Browser.Dom
import Browser.Events
import Color
import Dict
import Element
import Element.Background
import Element.Font
import Element.Input
import FeatureFlags
import Game
import GameMap
import Graphql.Http
import Html
import Html.Events
import Json.Decode
import Map
import Random
import Random.Dict
import Random.List
import RemoteData
import Route
import Session
import Task
import TroopCount
import ViewHelpers


type alias GameConfiguration =
    { numberOfPlayers : String
    , gameMapId : String
    , error : Maybe String
    , selectedMapId : String
    , maps : RemoteData.RemoteData (Graphql.Http.Error (List Map.Map)) (List Map.Map)
    }


type Model
    = ConfiguringGame GameConfiguration Session.Session
    | GeneratingRandomTroopCounts GameConfiguration Session.Session


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( ConfiguringGame { numberOfPlayers = "2", gameMapId = "1", error = Nothing, maps = RemoteData.NotAsked, selectedMapId = "" } session, Cmd.none )
        |> Tuple.mapSecond
            (\_ ->
                Cmd.batch
                    [ Task.attempt
                        (\viewportResult ->
                            case viewportResult of
                                Ok viewport ->
                                    WindowResized (round viewport.viewport.width) (round viewport.viewport.height)

                                Err _ ->
                                    WindowResized 0 0
                        )
                        Browser.Dom.getViewport
                    , Map.getAll GotMaps
                    ]
            )


toSession : Model -> Session.Session
toSession model =
    case model of
        ConfiguringGame _ session ->
            session

        GeneratingRandomTroopCounts _ session ->
            session



---- UPDATE ----


toGameConfiguration : Model -> GameConfiguration
toGameConfiguration model =
    case model of
        ConfiguringGame gameConfiguration _ ->
            gameConfiguration

        GeneratingRandomTroopCounts gameConfiguration _ ->
            gameConfiguration


type Msg
    = NumberOfPlayersChanged String
    | StartGameClicked
    | GotMaps (RemoteData.RemoteData (Graphql.Http.Error (List Map.Map)) (List Map.Map))
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | NumberOfPlayersKeyPressed Int
    | WindowResized Int Int
    | SelectMap String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame gameConfiguration session ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { gameConfiguration | numberOfPlayers = numberOfPlayers } session, Cmd.none )

                StartGameClicked ->
                    startGame session gameConfiguration

                NumberOfPlayersKeyPressed key ->
                    if key == 13 then
                        startGame session gameConfiguration

                    else
                        ( model, Cmd.none )

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( ConfiguringGame gameConfiguration { session | windowSize = Just { width = width, height = height } }, Cmd.none )

                GotMaps maps ->
                    ( ConfiguringGame { gameConfiguration | maps = maps } session, Cmd.none )

                SelectMap mapId ->
                    ( ConfiguringGame { gameConfiguration | selectedMapId = mapId } session, Cmd.none )

        GeneratingRandomTroopCounts gameConfiguration session ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    let
                        numberOfPlayers =
                            gameConfiguration.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    if FeatureFlags.isServerEnabled then
                        ( model, Game.create gameConfiguration.selectedMapId gameConfiguration.numberOfPlayers )

                    else
                        case ActiveGame.start session.gameMaps (GameMap.Id "1") numberOfPlayers neutralCountryTroopCounts of
                            Ok activeGame ->
                                ( GeneratingRandomTroopCounts
                                    gameConfiguration
                                    (session |> Session.addActiveGame (ActiveGame.Id "123") activeGame)
                                , Route.replaceUrl (Session.navKey session) (Route.ActiveGame (ActiveGame.Id "123"))
                                )

                            Err error ->
                                ( GeneratingRandomTroopCounts { gameConfiguration | error = Just (error |> ActiveGame.errorToString) } session, Cmd.none )

                NumberOfPlayersChanged _ ->
                    ( model, Cmd.none )

                StartGameClicked ->
                    ( model, Cmd.none )

                NumberOfPlayersKeyPressed _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( GeneratingRandomTroopCounts gameConfiguration (Session.updateWindowSize { width = width, height = height } session), Cmd.none )

                GotMaps _ ->
                    ( model, Cmd.none )

                SelectMap _ ->
                    ( model, Cmd.none )


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


startGame : Session.Session -> GameConfiguration -> ( Model, Cmd Msg )
startGame session gameConfiguration =
    case Dict.get gameConfiguration.gameMapId session.gameMaps of
        Just gameMap ->
            ( GeneratingRandomTroopCounts gameConfiguration session
            , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys gameMap.countries))
            )

        Nothing ->
            ( ConfiguringGame { gameConfiguration | error = Just "Couldn't find game map" } session, Cmd.none )


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String TroopCount.TroopCount)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        100
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (TroopCount.random maximumNeutralCountryTroops)



---- VIEW ----


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = ""
    , content =
        Element.layout [ Element.width Element.fill, Element.centerX ]
            (Element.column
                [ Element.width Element.fill, Element.centerX ]
                [ title
                , Element.el [ Element.centerX ]
                    (Element.column
                        [ Element.width Element.fill, Element.spacing 20 ]
                        [ numberOfPlayersInput (model |> toGameConfiguration |> .numberOfPlayers)
                        , mapSelect (model |> toGameConfiguration |> .maps) (model |> toGameConfiguration |> .selectedMapId)
                        , startGameButton
                        ]
                    )
                ]
            )
    }


mapSelect : RemoteData.RemoteData (Graphql.Http.Error (List Map.Map)) (List Map.Map) -> String -> Element.Element Msg
mapSelect mapsRemoteData selectedMapId =
    case mapsRemoteData of
        RemoteData.Success maps ->
            Element.Input.radio
                [ Element.padding 10
                , Element.spacing 20
                ]
                { onChange = SelectMap
                , selected = Just selectedMapId
                , label = Element.Input.labelAbove [] (Element.text "Select map")
                , options =
                    maps
                        |> List.map
                            (\map ->
                                Element.Input.option map.id (Element.text map.name)
                            )
                }

        RemoteData.Loading ->
            Element.text "Loading"

        RemoteData.Failure _ ->
            Element.text "fail"

        RemoteData.NotAsked ->
            Element.text "not asked"


numberOfPlayersInput : String -> Element.Element Msg
numberOfPlayersInput numberOfPlayers =
    Element.Input.text
        [ Element.width (Element.px 50)
        , Element.htmlAttribute
            (Html.Events.on
                "keyup"
                (Json.Decode.map NumberOfPlayersKeyPressed Html.Events.keyCode)
            )
        ]
        { onChange = NumberOfPlayersChanged
        , text = numberOfPlayers
        , placeholder = Nothing
        , label =
            Element.Input.labelLeft
                [ Element.centerY
                , Element.paddingEach { top = 0, left = 0, right = 10, bottom = 0 }
                ]
                (Element.text "Number of players")
        }


startGameButton : Element.Element Msg
startGameButton =
    Element.Input.button
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


title : Element.Element Msg
title =
    Element.el
        [ Element.padding 100
        , Element.Font.bold
        , Element.Font.size 80
        , Element.centerX
        , Element.Font.color (Color.darkBlue |> ViewHelpers.colorToElementColor)
        ]
        (Element.text "Fracas")



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
