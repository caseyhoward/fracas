module Page.NewGame exposing (Model, Msg, init, subscriptions, toSession, update, view)

-- import Map

import Browser.Dom
import Browser.Events
import Colors
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Game
import Graphql.Http
import Html
import Html.Events
import Json.Decode
import Map
import Player
import Random
import Random.Dict
import Random.List
import RemoteData
import Route
import Session
import Task
import TroopCount
import ViewHelpers


type alias NewGame =
    { players : Dict.Dict Int Player.NewPlayer
    , error : Maybe String
    , selectedMapId : String
    , maps : RemoteData.RemoteData (Graphql.Http.Error (List Map.Map)) (List Map.Map)
    , configureColor : Maybe Int
    }


type Model
    = ConfiguringGame NewGame Session.Session
    | GeneratingRandomTroopCounts NewGame Session.Session
    | Redirecting NewGame Session.Session


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( ConfiguringGame
        { players = Player.defaultNewPlayers
        , configureColor = Nothing
        , error = Nothing
        , maps = RemoteData.NotAsked
        , selectedMapId = ""
        }
        session
    , Cmd.none
    )
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

        Redirecting _ session ->
            session


toNewGame : Model -> NewGame
toNewGame model =
    case model of
        ConfiguringGame newGame _ ->
            newGame

        GeneratingRandomTroopCounts newGame _ ->
            newGame

        Redirecting newGame _ ->
            newGame



---- UPDATE ----


type Msg
    = AddPlayer
    | StartGameClicked
    | ColorSelected Int Colors.Color
    | ChangeColorButtonClicked Int
    | GotMaps (RemoteData.RemoteData (Graphql.Http.Error (List Map.Map)) (List Map.Map))
    | GameCreated (RemoteData.RemoteData (Graphql.Http.Error Game.Id) Game.Id)
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | WindowResized Int Int
    | SelectMap String
    | UpdatePlayerName Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame newGame session ->
            case msg of
                AddPlayer ->
                    Debug.todo ""

                ColorSelected playerId color ->
                    case newGame.players |> Dict.get playerId of
                        Just player ->
                            ( ConfiguringGame
                                { newGame
                                    | players =
                                        newGame.players |> Dict.insert playerId { player | color = color }
                                    , configureColor = Nothing
                                }
                                session
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                ChangeColorButtonClicked playerId ->
                    ( ConfiguringGame { newGame | configureColor = Just playerId } session, Cmd.none )

                StartGameClicked ->
                    startGame session newGame

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( ConfiguringGame newGame { session | windowSize = Just { width = width, height = height } }, Cmd.none )

                GotMaps maps ->
                    ( ConfiguringGame { newGame | maps = maps } session, Cmd.none )

                SelectMap mapId ->
                    ( ConfiguringGame { newGame | selectedMapId = mapId } session, Cmd.none )

                GameCreated _ ->
                    ( model, Cmd.none )

                UpdatePlayerName playerId name ->
                    case newGame.players |> Dict.get playerId of
                        Just player ->
                            ( ConfiguringGame { newGame | players = newGame.players |> Dict.insert playerId { player | name = name } } session, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

        GeneratingRandomTroopCounts newGame session ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    ( model, Game.create newGame.selectedMapId newGame.players neutralCountryTroopCounts GameCreated )

                AddPlayer ->
                    ( model, Cmd.none )

                ColorSelected _ _ ->
                    ( model, Cmd.none )

                ChangeColorButtonClicked _ ->
                    ( model, Cmd.none )

                StartGameClicked ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( GeneratingRandomTroopCounts newGame (Session.updateWindowSize { width = width, height = height } session), Cmd.none )

                GotMaps _ ->
                    ( model, Cmd.none )

                SelectMap _ ->
                    ( model, Cmd.none )

                GameCreated gameIdResult ->
                    case gameIdResult of
                        RemoteData.Success gameId ->
                            ( Redirecting newGame session, Route.pushUrl (Session.navKey session) (Route.Game gameId (Player.Id 1)) )

                        RemoteData.NotAsked ->
                            ( model, Cmd.none )

                        RemoteData.Loading ->
                            ( model, Cmd.none )

                        RemoteData.Failure error ->
                            ( ConfiguringGame { newGame | error = Just (ViewHelpers.errorToString error) } session, Cmd.none )

                UpdatePlayerName _ _ ->
                    ( model, Cmd.none )

        Redirecting _ _ ->
            ( model, Cmd.none )


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


startGame : Session.Session -> NewGame -> ( Model, Cmd Msg )
startGame session newGame =
    case newGame.maps of
        RemoteData.Success maps ->
            case maps |> List.filter (\map -> map.id == Map.Id newGame.selectedMapId) |> List.head of
                Just map ->
                    ( GeneratingRandomTroopCounts newGame session
                    , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys map.countries))
                    )

                Nothing ->
                    ( ConfiguringGame { newGame | error = Just "Couldn't find game map" } session, Cmd.none )

        _ ->
            ( ConfiguringGame { newGame | error = Just "This shouldn't happen" } session, Cmd.none )


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
        Element.layout [ Element.width Element.fill, Element.centerX, Element.height Element.fill ]
            (Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.centerX
                , Element.inFront (playerColorSelect (model |> toNewGame |> .players) (model |> toNewGame |> .configureColor))
                ]
                [ title
                , Element.el [ Element.centerX ]
                    (Element.column
                        [ Element.width Element.fill, Element.spacing 20 ]
                        [ playerConfiguration (model |> toNewGame |> .players)
                        , mapSelect (model |> toNewGame |> .maps) (model |> toNewGame |> .selectedMapId)
                        , startGameButton
                        ]
                    )
                ]
            )
    }


playerColorSelect : Dict.Dict Int Player.NewPlayer -> Maybe Int -> Element.Element Msg
playerColorSelect players maybePlayerId =
    case maybePlayerId of
        Just playerId ->
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color (Element.rgba255 0 0 0 0.8)
                ]
                (Element.wrappedRow [ Element.width (Element.px 200), Element.centerX, Element.centerY ]
                    (players
                        |> Player.availablePlayerColors
                        |> List.map (\color -> colorButton color (ColorSelected playerId color))
                    )
                )

        Nothing ->
            Element.none


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
                , label = Element.Input.labelAbove [ Element.Font.bold ] (Element.text "Map")
                , options =
                    maps
                        |> List.map
                            (\map ->
                                Element.Input.optionWith
                                    (map.id |> Map.idToString)
                                    (\optionState ->
                                        let
                                            border =
                                                case optionState of
                                                    Element.Input.Idle ->
                                                        [ Element.Border.color (Colors.gray |> Colors.toElementColor)
                                                        , Element.Border.solid
                                                        , Element.Border.width 2
                                                        ]

                                                    Element.Input.Focused ->
                                                        [ Element.Border.color (Colors.white |> Colors.toElementColor)
                                                        , Element.Border.solid
                                                        , Element.Border.width 2
                                                        ]

                                                    Element.Input.Selected ->
                                                        [ Element.Border.color (Colors.blue |> Colors.toElementColor)
                                                        , Element.Border.solid
                                                        , Element.Border.width 2
                                                        , Element.Background.color (Colors.lightBlue |> Colors.toElementColor)
                                                        , Element.Font.color (Colors.white |> Colors.toElementColor)
                                                        ]
                                        in
                                        Element.row
                                            (Element.spacing 10 :: Element.padding 10 :: Element.width (Element.px 300) :: border)
                                            [ Element.el
                                                [ Element.width (Element.px 50) ]
                                                (Map.view 100 map.countries map.dimensions |> Element.html)
                                            , Element.text map.name
                                            ]
                                    )
                            )
                }

        RemoteData.Loading ->
            Element.text "..."

        RemoteData.Failure _ ->
            Element.text "fail"

        RemoteData.NotAsked ->
            Element.text ""


playerConfiguration : Dict.Dict Int Player.NewPlayer -> Element.Element Msg
playerConfiguration players =
    Element.column [ Element.spacing 20 ]
        (Element.el [ Element.Font.bold ] (Element.text "Players")
            :: (players
                    |> Dict.map playerFields
                    |> Dict.values
               )
        )


playerFields : Int -> Player.NewPlayer -> Element.Element Msg
playerFields playerId player =
    Element.row []
        [ Element.Input.text
            [ Element.width (Element.px 250)
            ]
            { onChange = UpdatePlayerName playerId
            , text = player.name
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Name"
            }
        , colorButton player.color (ChangeColorButtonClicked playerId)
        ]


colorButton : Colors.Color -> Msg -> Element.Element Msg
colorButton color message =
    Element.Input.button
        (ViewHelpers.defaultButtonAttributes
            ++ [ Element.Background.color (color |> Colors.toElementColor)
               , Element.height Element.fill
               , Element.width (Element.px 50)
               ]
        )
        { onPress = Just message, label = Element.text "" }


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
               , Element.Font.color (Colors.white |> ViewHelpers.colorToElementColor)
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
        , Element.Font.color (Colors.darkBlue |> ViewHelpers.colorToElementColor)
        ]
        (Element.text "Fracas")



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
