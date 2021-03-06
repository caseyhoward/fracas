module Page.NewGame exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Browser.Dom
import Browser.Events
import Colors
import DefaultMap
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Lazy
import Graphql.Http
import Html
import Html.Attributes
import InternetGame
import LocalGame
import Map
import NewGame
import Player
import Random
import Random.Dict
import Random.List
import RemoteData
import Route
import Session
import Task
import TroopCount
import UserMap
import ViewHelpers


type alias NewGame =
    { players : Dict.Dict String Player.NewPlayer
    , error : Maybe String
    , selectedMapId : Maybe UserMap.Id
    , maps : RemoteData.RemoteData (Graphql.Http.Error (List UserMap.UserMap)) (List UserMap.UserMap)
    , defaultMaps : List DefaultMap.DefaultMap
    , configureColor : Maybe String
    }


type Model
    = ChoosingGameType Session.Session
    | LocalGame LocalGameCase


type LocalGameCase
    = ConfiguringGame NewGame Session.Session
    | GeneratingRandomTroopCounts NewGame Session.Session
    | Redirecting NewGame Session.Session


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( ChoosingGameType session
    , Cmd.none
    )


toSession : Model -> Session.Session
toSession model =
    case model of
        ChoosingGameType session ->
            session

        LocalGame (ConfiguringGame _ session) ->
            session

        LocalGame (GeneratingRandomTroopCounts _ session) ->
            session

        LocalGame (Redirecting _ session) ->
            session


toNewGame : LocalGameCase -> NewGame
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
    | ColorSelectBackgroundClicked
    | LocalGameClicked
    | InternetGameClicked
    | InternetGameCreated (RemoteData.RemoteData (Graphql.Http.Error InternetGame.PlayerToken) InternetGame.PlayerToken)
    | FocusResult (Result Browser.Dom.Error ())
    | RemovePlayer String
    | StartGameClicked
    | ColorSelected String Colors.Color
    | ChangeColorButtonClicked String
    | GotMaps (RemoteData.RemoteData (Graphql.Http.Error (List UserMap.UserMap)) (List UserMap.UserMap))
    | GameCreated (RemoteData.RemoteData (Graphql.Http.Error LocalGame.Id) LocalGame.Id)
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | WindowResized Int Int
    | SelectMap UserMap.Id
    | UpdatePlayerName String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ChoosingGameType session ->
            case msg of
                InternetGameClicked ->
                    ( model, InternetGame.create session.apiUrl InternetGameCreated )

                LocalGameClicked ->
                    ( LocalGame
                        (ConfiguringGame
                            { players = Player.defaultNewPlayers
                            , configureColor = Nothing
                            , error = Nothing
                            , maps = RemoteData.Loading
                            , defaultMaps = DefaultMap.all
                            , selectedMapId = Nothing
                            }
                            session
                        )
                    , Cmd.batch
                        [ Task.attempt
                            (\viewportResult ->
                                case viewportResult of
                                    Ok viewport ->
                                        WindowResized (round viewport.viewport.width) (round viewport.viewport.height)

                                    Err _ ->
                                        WindowResized 0 0
                            )
                            Browser.Dom.getViewport
                        , UserMap.getAll session.apiUrl GotMaps
                        ]
                    )

                InternetGameCreated playerTokenRemoteData ->
                    case playerTokenRemoteData of
                        RemoteData.Success playerToken ->
                            ( ChoosingGameType session, Route.pushUrl (Session.navKey session) (Route.InternetGameConfiguration playerToken) )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        LocalGame localGameCase ->
            updateLocalGame msg localGameCase
                |> Tuple.mapFirst LocalGame


updateLocalGame : Msg -> LocalGameCase -> ( LocalGameCase, Cmd Msg )
updateLocalGame msg model =
    case model of
        ConfiguringGame newGame session ->
            case msg of
                AddPlayer ->
                    case Player.availablePlayerColors newGame.players |> List.head of
                        Just color ->
                            let
                                updatedPlayers =
                                    (newGame.players |> Dict.values)
                                        ++ [ { name = "", color = color } ]
                                        |> List.indexedMap (\id player -> ( id |> String.fromInt, player ))
                                        |> Dict.fromList

                                newId =
                                    (updatedPlayers |> Dict.keys |> List.length) - 1
                            in
                            ( ConfiguringGame
                                { newGame
                                    | players = updatedPlayers
                                }
                                session
                            , Browser.Dom.focus ("player-name-" ++ String.fromInt newId) |> Task.attempt FocusResult
                            )

                        Nothing ->
                            ( ConfiguringGame { newGame | error = Just "Too many cooks" } session, Cmd.none )

                ColorSelectBackgroundClicked ->
                    ( ConfiguringGame { newGame | configureColor = Nothing } session, Cmd.none )

                RemovePlayer playerId ->
                    ( ConfiguringGame
                        { newGame
                            | players = newGame.players |> Dict.remove playerId
                        }
                        session
                    , Cmd.none
                    )

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

                FocusResult _ ->
                    ( model, Cmd.none )

                ChangeColorButtonClicked playerId ->
                    ( ConfiguringGame { newGame | configureColor = Just playerId } session, Cmd.none )

                StartGameClicked ->
                    startGame session newGame

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( ConfiguringGame newGame { session | windowSize = { width = width, height = height } }, Cmd.none )

                GotMaps maps ->
                    ( ConfiguringGame
                        { newGame
                            | maps = maps
                            , selectedMapId =
                                case maps of
                                    RemoteData.Success allMaps ->
                                        allMaps |> List.head |> Maybe.map .id

                                    _ ->
                                        Nothing
                        }
                        session
                    , Cmd.none
                    )

                SelectMap mapId ->
                    ( ConfiguringGame { newGame | selectedMapId = Just mapId } session, Cmd.none )

                GameCreated _ ->
                    ( model, Cmd.none )

                UpdatePlayerName playerId name ->
                    case newGame.players |> Dict.get playerId of
                        Just player ->
                            ( ConfiguringGame { newGame | players = newGame.players |> Dict.insert playerId { player | name = name } } session, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                LocalGameClicked ->
                    ( model, Cmd.none )

                InternetGameClicked ->
                    ( model, Cmd.none )

                InternetGameCreated _ ->
                    ( model, Cmd.none )

        GeneratingRandomTroopCounts newGame session ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    case newGame.selectedMapId of
                        Just mapId ->
                            ( model, LocalGame.create session.apiUrl (mapId |> UserMap.idToString) newGame.players neutralCountryTroopCounts GameCreated )

                        Nothing ->
                            ( model, Cmd.none )

                AddPlayer ->
                    ( model, Cmd.none )

                ColorSelected _ _ ->
                    ( model, Cmd.none )

                ColorSelectBackgroundClicked ->
                    ( model, Cmd.none )

                ChangeColorButtonClicked _ ->
                    ( model, Cmd.none )

                FocusResult _ ->
                    ( model, Cmd.none )

                RemovePlayer _ ->
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
                            ( Redirecting newGame session, Route.pushUrl (Session.navKey session) (Route.LocalGame gameId (Player.Id "1")) )

                        RemoteData.NotAsked ->
                            ( model, Cmd.none )

                        RemoteData.Loading ->
                            ( model, Cmd.none )

                        RemoteData.Failure error ->
                            ( ConfiguringGame { newGame | error = Just (ViewHelpers.errorToString error) } session, Cmd.none )

                UpdatePlayerName _ _ ->
                    ( model, Cmd.none )

                LocalGameClicked ->
                    ( model, Cmd.none )

                InternetGameClicked ->
                    ( model, Cmd.none )

                InternetGameCreated _ ->
                    ( model, Cmd.none )

        Redirecting _ _ ->
            ( model, Cmd.none )


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


startGame : Session.Session -> NewGame -> ( LocalGameCase, Cmd Msg )
startGame session newGame =
    case newGame.maps of
        RemoteData.Success maps ->
            case newGame.selectedMapId of
                Just mapId ->
                    case maps |> List.filter (\map -> map.id == mapId) |> List.head of
                        Just map ->
                            ( GeneratingRandomTroopCounts newGame session
                            , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys map.map.countries))
                            )

                        Nothing ->
                            ( ConfiguringGame { newGame | error = Just "Couldn't find game map" } session, Cmd.none )

                Nothing ->
                    ( ConfiguringGame { newGame | error = Just "This shouldn't happen" } session, Cmd.none )

        _ ->
            ( ConfiguringGame { newGame | error = Just "This shouldn't happen" } session, Cmd.none )


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String TroopCount.TroopCount)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        (List.length countryIds * randomTroopCountryPercentage // 100)
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (TroopCount.random maximumNeutralCountryTroops)


randomTroopCountryPercentage : Int
randomTroopCountryPercentage =
    50



---- VIEW ----


view : Model -> { title : String, content : Html.Html Msg }
view model =
    case model of
        ChoosingGameType _ ->
            viewChoosingGameType

        LocalGame localGame ->
            viewLocalGame localGame


viewChoosingGameType : { title : String, content : Html.Html Msg }
viewChoosingGameType =
    let
        gameTypeButton : String -> String -> Msg -> Element.Element Msg
        gameTypeButton titleText description msg =
            Element.el [ Element.width Element.fill ]
                (Element.el
                    [ Element.centerX
                    ]
                    (Element.Input.button
                        (ViewHelpers.defaultButtonAttributes
                            ++ [ Element.width (Element.px 300)
                               , Element.height Element.fill
                               , Element.centerX
                               , Element.Border.color (Colors.black |> Colors.toElementColor)
                               , Element.Border.rounded 10
                               ]
                        )
                        { label =
                            Element.el
                                [ Element.padding 20, Element.width Element.fill, Element.Font.center ]
                                (Element.column
                                    [ Element.width Element.fill, Element.spacing 20 ]
                                    [ Element.el [ Element.Font.size 20, Element.width Element.fill, Element.Font.center ] (Element.text titleText)
                                    , Element.el [ Element.Font.medium ] (Element.paragraph [] [ Element.text description ])
                                    ]
                                )
                        , onPress = Just msg
                        }
                    )
                )
    in
    { title = "Choose Game Type"
    , content =
        layout
            Element.none
            (Element.el [ Element.width Element.fill, Element.centerX ]
                (Element.wrappedRow
                    [ Element.spacing 50, Element.centerX ]
                    [ gameTypeButton "Local" "All players will be using this computer or device" LocalGameClicked
                    , gameTypeButton "Internet" "Each player will be using their own computer or device" InternetGameClicked
                    ]
                )
            )
    }


viewLocalGame : LocalGameCase -> { title : String, content : Html.Html Msg }
viewLocalGame model =
    { title = "Configure Local Game"
    , content =
        layout
            (playerColorSelect (model |> toNewGame |> .players) (model |> toNewGame |> .configureColor))
            (Element.column
                [ Element.width Element.fill
                , Element.spacingXY 0 20
                , Element.Background.color (Colors.blue |> Colors.toElementColor)
                ]
                [ Element.el [ Element.width Element.fill, Element.centerX ]
                    (Element.wrappedRow
                        [ Element.spacing 40, Element.centerX ]
                        [ Element.el
                            [ Element.alignTop, Element.height Element.fill, Element.width Element.fill ]
                            (playerConfiguration (model |> toNewGame |> .players))
                        , Element.el
                            [ Element.alignTop, Element.height Element.fill, Element.width Element.fill ]
                            (mapConfiguration (model |> toNewGame |> .maps) (model |> toNewGame |> .selectedMapId))
                        ]
                    )
                , Element.el [ Element.width Element.fill ] (NewGame.startGameButton StartGameClicked)
                ]
            )
    }


layout : Element.Element Msg -> Element.Element Msg -> Html.Html Msg
layout overlay body =
    Element.layout
        (ViewHelpers.layoutAttributes
            ++ [ Element.inFront overlay
               ]
        )
        (Element.column
            [ Element.width Element.fill
            , Element.spacingXY 0 20
            ]
            [ Element.el [ Element.width Element.fill, Element.centerX ] ViewHelpers.title
            , body
            ]
        )


playerColorSelect : Dict.Dict String Player.NewPlayer -> Maybe String -> Element.Element Msg
playerColorSelect players maybePlayerId =
    case maybePlayerId of
        Just playerId ->
            case Dict.get playerId players of
                Just player ->
                    ViewHelpers.dialog
                        ColorSelectBackgroundClicked
                        [ Element.width Element.shrink, Element.height (Element.px 300) ]
                        (Element.column
                            [ Element.padding 20
                            , Element.Background.color (Colors.white |> Colors.toElementColor)
                            , Element.spacing 20
                            , Element.width (Element.px 300)
                            , Element.height Element.fill
                            ]
                            [ Element.text ("Select color for " ++ player.name)
                            , Element.wrappedRow [ Element.width Element.fill ]
                                (players
                                    |> Player.availablePlayerColors
                                    |> List.map (\color -> Element.el [ Element.height (Element.px 50) ] (NewGame.colorButton color (ColorSelected playerId color)))
                                )
                            ]
                        )

                Nothing ->
                    Element.text "Error getting player"

        Nothing ->
            Element.none


mapConfiguration : RemoteData.RemoteData (Graphql.Http.Error (List UserMap.UserMap)) (List UserMap.UserMap) -> Maybe UserMap.Id -> Element.Element Msg
mapConfiguration mapsRemoteData selectedMapId =
    case mapsRemoteData of
        RemoteData.Success maps ->
            Element.Lazy.lazy2 mapSelect maps selectedMapId

        RemoteData.Loading ->
            Element.el
                [ Element.centerX
                ]
                (Element.el
                    [ Element.width (Element.px 340)
                    , Element.Background.color (Colors.gray |> Colors.toElementColor)
                    , Element.Border.rounded 10
                    , Element.padding 10
                    ]
                    (Element.text "Loading maps ...")
                )

        RemoteData.Failure _ ->
            Element.text "fail"

        RemoteData.NotAsked ->
            Element.text "not asked"


mapSelect : List UserMap.UserMap -> Maybe UserMap.Id -> Element.Element Msg
mapSelect userMaps selectedMapId =
    Element.el
        [ Element.centerX
        , Element.Background.color (Colors.gray |> Colors.toElementColor)
        , Element.Border.rounded 10
        , Element.padding 20
        ]
        (Element.Input.radio
            [ Element.padding 8
            , Element.spacing 20
            ]
            { onChange = SelectMap
            , selected = selectedMapId
            , label = Element.Input.labelAbove [ Element.Font.bold ] (Element.text "Map")
            , options =
                userMaps
                    |> List.map
                        (\userMap ->
                            Element.Input.optionWith
                                userMap.id
                                (\optionState ->
                                    let
                                        border =
                                            case optionState of
                                                Element.Input.Idle ->
                                                    [ Element.Border.color (Colors.charcoal |> Colors.toElementColor)
                                                    , Element.Background.color (Colors.white |> Colors.toElementColor)
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
                                            (Element.Lazy.lazy2 NewGame.mapView userMap.map.countries userMap.map.dimensions)
                                        , Element.text userMap.map.name
                                        ]
                                )
                        )
            }
        )


playerConfiguration : Dict.Dict String Player.NewPlayer -> Element.Element Msg
playerConfiguration players =
    Element.column
        [ Element.spacing 20
        , Element.centerX
        , Element.Background.color (Colors.gray |> Colors.toElementColor)
        , Element.padding 20
        , Element.Border.rounded 10
        ]
        ((Element.el [ Element.Font.bold ] (Element.text "Players")
            :: (players
                    |> Dict.map (playerFields (Dict.size players))
                    |> Dict.values
               )
         )
            ++ [ NewGame.addPlayerButton AddPlayer ]
        )


playerFields : Int -> String -> Player.NewPlayer -> Element.Element Msg
playerFields numberOfPlayers playerId player =
    Element.row [ Element.spacing 10 ]
        [ Element.row []
            [ Element.Input.text
                [ Element.width (Element.px 200)
                , Html.Attributes.id ("player-name-" ++ playerId) |> Element.htmlAttribute
                ]
                { onChange = UpdatePlayerName playerId
                , text = player.name
                , placeholder = Nothing
                , label = Element.Input.labelHidden "Name"
                }
            , NewGame.colorButton player.color (ChangeColorButtonClicked playerId)
            ]
        , if numberOfPlayers >= 3 then
            NewGame.removePlayerButton playerId RemovePlayer

          else
            Element.el [ NewGame.removePlayerButtonWidth ] (Element.text "")
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\x y -> WindowResized x y)
