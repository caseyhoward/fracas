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
import GameMap
import Html
import Html.Events
import Json.Decode
import Maps.Big
import Random
import Random.Dict
import Random.List
import Route
import Session
import Task
import TroopCount
import ViewHelpers


type alias GameConfiguration =
    { numberOfPlayers : String
    , gameMapId : String
    }


type Model
    = ConfiguringGame GameConfiguration Session.Session
    | GeneratingRandomTroopCounts GameConfiguration Session.Session


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( ConfiguringGame { numberOfPlayers = "2", gameMapId = "1" } session, Cmd.none )
        |> Tuple.mapSecond
            (\_ ->
                Task.attempt
                    (\viewportResult ->
                        case viewportResult of
                            Ok viewport ->
                                WindowResized (round viewport.viewport.width) (round viewport.viewport.height)

                            Err _ ->
                                WindowResized 0 0
                    )
                    Browser.Dom.getViewport
            )



---- UPDATE ----


type Msg
    = NumberOfPlayersChanged String
    | StartGameClicked
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | NumberOfPlayersKeyPressed Int
    | WindowResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame gameConfiguration session ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { gameConfiguration | numberOfPlayers = numberOfPlayers } session, Cmd.none )

                StartGameClicked ->
                    startGame session model gameConfiguration

                NumberOfPlayersKeyPressed key ->
                    if key == 13 then
                        startGame session model gameConfiguration

                    else
                        ( model, Cmd.none )

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( ConfiguringGame gameConfiguration { session | windowSize = Just { width = width, height = height } }, Cmd.none )

        GeneratingRandomTroopCounts gameConfiguration session ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    let
                        numberOfPlayers =
                            gameConfiguration.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    case ActiveGame.start session.gameMaps (GameMap.Id "1") numberOfPlayers neutralCountryTroopCounts of
                        Ok activeGame ->
                            ( GeneratingRandomTroopCounts
                                gameConfiguration
                                (session |> Session.addActiveGame (ActiveGame.Id "123") activeGame)
                            , Route.replaceUrl (Session.navKey session) (Route.ActiveGame (ActiveGame.Id "123"))
                            )

                        Err error ->
                            Debug.todo ""

                NumberOfPlayersChanged _ ->
                    ( model, Cmd.none )

                StartGameClicked ->
                    ( model, Cmd.none )

                NumberOfPlayersKeyPressed _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( GeneratingRandomTroopCounts gameConfiguration (Session.updateWindowSize { width = width, height = height } session), Cmd.none )


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


startGame : Session.Session -> Model -> GameConfiguration -> ( Model, Cmd Msg )
startGame session model gameConfiguration =
    case Dict.get gameConfiguration.gameMapId session.gameMaps of
        Just gameMap ->
            ( GeneratingRandomTroopCounts gameConfiguration session
            , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys gameMap.countries))
            )

        Nothing ->
            Debug.todo ""



-- ConfiguringGame { gameConfiguration | error = Just "Couldn't find game map" }


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String TroopCount.TroopCount)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        100
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (TroopCount.random maximumNeutralCountryTroops)



---- VIEW ----


toGameConfiguration : Model -> GameConfiguration
toGameConfiguration model =
    case model of
        ConfiguringGame gameConfiguration _ ->
            gameConfiguration

        GeneratingRandomTroopCounts gameConfiguration _ ->
            gameConfiguration


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = ""
    , content =
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
                                    (Json.Decode.map NumberOfPlayersKeyPressed Html.Events.keyCode)
                                )
                            ]
                            { onChange = NumberOfPlayersChanged
                            , text = model |> toGameConfiguration |> .numberOfPlayers
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
    }


toSession : Model -> Session.Session
toSession model =
    case model of
        ConfiguringGame _ session ->
            session

        GeneratingRandomTroopCounts _ session ->
            session



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
