module Main exposing (BorderSegment, Country, coordinatesToPolygon, getEdgesForArea, getMapDimensions, getNeighborCoordinates, main, parseMap, parseRawMap, updateCountry)

import Browser
import Collage
import Collage.Events
import Collage.Render
import Collage.Text
import Color
import Dict
import Element
import Element.Background
import Element.Border
import Element.Input
import Html exposing (Html)
import Maps.Big
import Maps.Simple
import Maps.SuperSimple
import Set
import Time



-- Settings


troopsPerCountryPerTurn : Int
troopsPerCountryPerTurn =
    1


defaultScale : Int
defaultScale =
    12


defaultPlayerColors : Dict.Dict Int Color.Color
defaultPlayerColors =
    Dict.fromList
        [ ( 1, Color.lightRed )
        , ( 2, Color.lightPurple )
        , ( 3, Color.lightYellow )
        , ( 4, Color.lightGreen )
        , ( 5, Color.lightOrange )
        , ( 6, Color.brown )
        ]



---- MODEL ----


type alias GameMap =
    { countries : Dict.Dict String Country
    , bodiesOfWater : Dict.Dict String BodyOfWater
    , dimensions : ( Float, Float )
    }


type alias Country =
    { coordinates : Set.Set ( Int, Int )
    , polygon : List ( Float, Float )
    , center : ( Int, Int )
    , borderEdges : Set.Set BorderSegment
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias BodyOfWater =
    { coordinates : Set.Set ( Int, Int )
    , borderEdges : Set.Set BorderSegment
    , polygon : List ( Float, Float )
    , neighboringCountries : Set.Set String
    }


type alias RawGameMap =
    Dict.Dict ( Int, Int ) String


type alias PlayerCountry =
    { population : Int
    }


type Model
    = ConfiguringGame ConfigurationAttributes
    | LoadingGame Int ConfigurationAttributes
    | PlayingGame PlayingGameAttributes


type alias ConfigurationAttributes =
    { numberOfPlayers : String }


type alias PlayingGameAttributes =
    { currentPlayerTurn : PlayerTurn
    , map : GameMap
    , players : Dict.Dict Int Player
    , error : Maybe String
    , numberOfPlayers : Int
    }


type PlayerTurn
    = PlayerTurn Int PlayerTurnStage


type PlayerTurnStage
    = CapitolPlacement
    | TroopPlacement Int
    | AttackAnnexOrPort
    | TroopMovement
    | TroopMovementFromSelected String
    | GameOver Int


type alias Player =
    { name : String
    , countries : Dict.Dict String PlayerCountry
    , capitolStatus : CapitolStatus
    , color : Color.Color
    }


type CapitolStatus
    = NoCapitol
    | Capitol String (Set.Set ( Float, Float ))


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( ConfiguringGame { numberOfPlayers = "6" }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = CountryClicked String
    | NumberOfPlayersChanged String
    | StartGameClicked
    | Pass
    | LoadGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame configurationOptions ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { configurationOptions | numberOfPlayers = numberOfPlayers }, Cmd.none )

                StartGameClicked ->
                    ( LoadingGame 0 configurationOptions, Cmd.none )

                Pass ->
                    ( model, Cmd.none )

                CountryClicked _ ->
                    ( model, Cmd.none )

                LoadGame ->
                    ( model, Cmd.none )

        LoadingGame counter configurationOptions ->
            case msg of
                LoadGame ->
                    let
                        numberOfPlayers =
                            configurationOptions.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    if counter > 0 then
                        ( PlayingGame
                            { map = parseMap Maps.Big.map
                            , players =
                                List.range 1 numberOfPlayers
                                    |> List.map
                                        (\playerId ->
                                            ( playerId
                                            , { countries = Dict.empty
                                              , name = "Player " ++ String.fromInt playerId
                                              , capitolStatus = NoCapitol
                                              , color = getDefaultColor playerId
                                              }
                                            )
                                        )
                                    |> Dict.fromList
                            , currentPlayerTurn = PlayerTurn 1 CapitolPlacement
                            , error = Nothing
                            , numberOfPlayers = numberOfPlayers
                            }
                        , Cmd.none
                        )
                        -- ( LoadingGame (counter + 1) configurationOptions, Cmd.none )

                    else
                        ( LoadingGame (counter + 1) configurationOptions, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayingGame attributes ->
            case msg of
                CountryClicked id ->
                    case Dict.get id attributes.map.countries of
                        Just country ->
                            case attributes.currentPlayerTurn of
                                PlayerTurn playerId _ ->
                                    case Dict.get playerId attributes.players of
                                        Just _ ->
                                            ( PlayingGame (handleCountryClickFromPlayer id country attributes)
                                            , Cmd.none
                                            )

                                        Nothing ->
                                            ( PlayingGame attributes, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Pass ->
                    case attributes.currentPlayerTurn of
                        PlayerTurn _ (TroopMovementFromSelected _) ->
                            ( PlayingGame
                                { attributes
                                    | currentPlayerTurn =
                                        nextPlayerTurn attributes.numberOfPlayers "-1" attributes.players attributes.currentPlayerTurn

                                    -- Gross
                                    , error = Nothing
                                }
                            , Cmd.none
                            )

                        PlayerTurn _ TroopMovement ->
                            ( PlayingGame
                                { attributes
                                    | currentPlayerTurn =
                                        attributes.currentPlayerTurn
                                            -- Gross
                                            |> nextPlayerTurn attributes.numberOfPlayers "-1" attributes.players
                                            -- More gross
                                            |> nextPlayerTurn attributes.numberOfPlayers "-1" attributes.players
                                    , error = Nothing
                                }
                            , Cmd.none
                            )

                        PlayerTurn _ AttackAnnexOrPort ->
                            ( PlayingGame
                                { attributes
                                    | currentPlayerTurn = nextPlayerTurn attributes.numberOfPlayers "-1" attributes.players attributes.currentPlayerTurn -- Gross
                                    , error = Nothing
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                NumberOfPlayersChanged _ ->
                    ( model, Cmd.none )

                StartGameClicked ->
                    ( model, Cmd.none )

                LoadGame ->
                    ( model, Cmd.none )


getDefaultColor : Int -> Color.Color
getDefaultColor playerId =
    case Dict.get playerId defaultPlayerColors of
        Just color ->
            color

        Nothing ->
            Color.black


handleCountryClickFromPlayer : String -> Country -> PlayingGameAttributes -> PlayingGameAttributes
handleCountryClickFromPlayer clickedCountryId country model =
    case model.currentPlayerTurn of
        PlayerTurn playerId playerTurnStage ->
            case Dict.get playerId model.players of
                Just currentPlayer ->
                    case playerTurnStage of
                        CapitolPlacement ->
                            case getCountryStatus clickedCountryId currentPlayer model.players of
                                OccupiedByCurrentPlayer _ ->
                                    { model | error = Just "Error: Somehow you are placing a second capitol" }

                                OccupiedByOpponent _ _ _ ->
                                    { model | error = Just "You must select an unoccuppied country" }

                                Unoccupied ->
                                    let
                                        updatedPlayer =
                                            { currentPlayer
                                                | countries =
                                                    Dict.insert clickedCountryId { population = 0 } currentPlayer.countries
                                                , capitolStatus = Capitol clickedCountryId (capitolDotsCoordinates country.coordinates defaultScale)
                                            }
                                    in
                                    { model
                                        | players = Dict.insert playerId updatedPlayer model.players
                                        , currentPlayerTurn = nextPlayerTurn model.numberOfPlayers clickedCountryId model.players model.currentPlayerTurn
                                        , error = Nothing
                                    }

                        TroopPlacement numberOfTroops ->
                            case getCountryStatus clickedCountryId currentPlayer model.players of
                                OccupiedByCurrentPlayer playerCountry ->
                                    let
                                        updatedPlayer =
                                            { currentPlayer
                                                | countries =
                                                    Dict.insert
                                                        clickedCountryId
                                                        { playerCountry | population = playerCountry.population + numberOfTroops }
                                                        currentPlayer.countries
                                            }

                                        updatedPlayers =
                                            Dict.insert playerId updatedPlayer model.players
                                    in
                                    { model
                                        | players = updatedPlayers
                                        , currentPlayerTurn = nextPlayerTurn model.numberOfPlayers clickedCountryId updatedPlayers model.currentPlayerTurn
                                        , error = Nothing
                                    }

                                OccupiedByOpponent _ _ _ ->
                                    { model | error = Just "You must put troops in your own country" }

                                Unoccupied ->
                                    { model | error = Just "You must put troops in your own country" }

                        AttackAnnexOrPort ->
                            case getCountryStatus clickedCountryId currentPlayer model.players of
                                OccupiedByCurrentPlayer playerCountry ->
                                    { model
                                        | error = Just "TODO: Implement building port"
                                    }

                                OccupiedByOpponent opponentPlayerId opponentPlayer opponentPlayerCountry ->
                                    let
                                        ( attackStrength, defenseStrength ) =
                                            country.neighboringCountries
                                                |> Set.foldl
                                                    (\neighboringCountryId ( attack, defense ) ->
                                                        case getCountryStatus neighboringCountryId currentPlayer model.players of
                                                            OccupiedByCurrentPlayer neighboringPlayerCountry ->
                                                                ( attack + neighboringPlayerCountry.population, defense )

                                                            OccupiedByOpponent neigborPlayerId _ neighboringPlayerCountry ->
                                                                if neigborPlayerId == opponentPlayerId then
                                                                    ( attack, defense + neighboringPlayerCountry.population )

                                                                else
                                                                    ( attack, defense )

                                                            _ ->
                                                                ( attack, defense )
                                                    )
                                                    ( 0, opponentPlayerCountry.population )

                                        remainingTroops =
                                            opponentPlayerCountry.population + defenseStrength - attackStrength
                                    in
                                    if attackStrength > defenseStrength then
                                        let
                                            ( updatedOpponentPlayer, updatedCurrentPlayer ) =
                                                if remainingTroops > 0 then
                                                    ( { opponentPlayer
                                                        | countries =
                                                            opponentPlayer.countries
                                                                |> Dict.insert
                                                                    clickedCountryId
                                                                    { opponentPlayerCountry | population = remainingTroops }
                                                      }
                                                    , currentPlayer
                                                    )

                                                else
                                                    ( if isCountryIdCapitol opponentPlayer clickedCountryId then
                                                        { opponentPlayer
                                                            | countries =
                                                                Dict.empty
                                                            , capitolStatus = NoCapitol
                                                        }

                                                      else
                                                        { opponentPlayer
                                                            | countries =
                                                                opponentPlayer.countries
                                                                    |> Dict.remove clickedCountryId
                                                        }
                                                    , { currentPlayer
                                                        | countries =
                                                            currentPlayer.countries
                                                                |> Dict.insert
                                                                    clickedCountryId
                                                                    { population = 0 }
                                                      }
                                                    )

                                            updatedPlayers =
                                                model.players
                                                    |> Dict.insert opponentPlayerId updatedOpponentPlayer
                                                    |> Dict.insert playerId updatedCurrentPlayer
                                        in
                                        { model
                                            | players = updatedPlayers
                                            , currentPlayerTurn = nextPlayerTurn model.numberOfPlayers clickedCountryId updatedPlayers model.currentPlayerTurn
                                            , error = Nothing
                                        }

                                    else
                                        { model
                                            | error = Just ("Not enough to attack (" ++ String.fromInt attackStrength ++ " < " ++ String.fromInt defenseStrength ++ ")")
                                        }

                                Unoccupied ->
                                    if canAnnexCountry model.map currentPlayer clickedCountryId then
                                        let
                                            updatedPlayer =
                                                { currentPlayer
                                                    | countries =
                                                        Dict.insert clickedCountryId { population = 0 } currentPlayer.countries
                                                }
                                        in
                                        { model
                                            | players = Dict.insert playerId updatedPlayer model.players
                                            , currentPlayerTurn = nextPlayerTurn model.numberOfPlayers clickedCountryId model.players model.currentPlayerTurn
                                            , error = Nothing
                                        }

                                    else
                                        { model
                                            | error = Just "You can't annex that country"
                                        }

                        TroopMovement ->
                            case getCountryStatus clickedCountryId currentPlayer model.players of
                                OccupiedByCurrentPlayer _ ->
                                    case Dict.get clickedCountryId currentPlayer.countries of
                                        Just playerCountry ->
                                            if playerCountry.population > 0 then
                                                { model
                                                    | currentPlayerTurn = nextPlayerTurn model.numberOfPlayers clickedCountryId model.players model.currentPlayerTurn
                                                    , error = Nothing
                                                }

                                            else
                                                { model | error = Just "Select a country with troops" }

                                        Nothing ->
                                            { model | error = Just "This shouldn't happen" }

                                _ ->
                                    { model | error = Just "You must move troops from your own country" }

                        TroopMovementFromSelected fromCountryId ->
                            case ( getCountryStatus fromCountryId currentPlayer model.players, getCountryStatus clickedCountryId currentPlayer model.players ) of
                                ( OccupiedByCurrentPlayer playerCountryFrom, OccupiedByCurrentPlayer playerCountryTo ) ->
                                    if areCountriesConnected model.map fromCountryId clickedCountryId then
                                        let
                                            updatedPlayer =
                                                { currentPlayer
                                                    | countries =
                                                        currentPlayer.countries
                                                            |> Dict.insert
                                                                fromCountryId
                                                                { playerCountryFrom | population = 0 }
                                                            |> Dict.insert
                                                                clickedCountryId
                                                                { playerCountryFrom | population = playerCountryFrom.population + playerCountryTo.population }
                                                }

                                            updatedPlayers =
                                                Dict.insert playerId updatedPlayer model.players
                                        in
                                        { model
                                            | players = updatedPlayers
                                            , currentPlayerTurn = nextPlayerTurn model.numberOfPlayers clickedCountryId updatedPlayers model.currentPlayerTurn
                                            , error = Nothing
                                        }

                                    else
                                        { model | error = Just "You can't move troops between those countries" }

                                _ ->
                                    { model | error = Just "You must move troops to your own country" }

                        GameOver _ ->
                            { model | error = Nothing }

                Nothing ->
                    model


areCountriesConnected : GameMap -> String -> String -> Bool
areCountriesConnected gameMap fromCountryId toCountryId =
    case Dict.get fromCountryId gameMap.countries of
        Just fromCountry ->
            Set.member toCountryId fromCountry.neighboringCountries

        Nothing ->
            False


canAnnexCountry : GameMap -> Player -> String -> Bool
canAnnexCountry gameMap player countryIdToAnnex =
    let
        playerCountries : List Country
        playerCountries =
            player.countries
                |> Dict.foldl
                    (\playerCountryId _ countries ->
                        case Dict.get playerCountryId gameMap.countries of
                            Just country ->
                                country :: countries

                            Nothing ->
                                countries
                    )
                    []

        playerNeighborCountries : List String
        playerNeighborCountries =
            playerCountries
                |> List.map
                    (\country ->
                        country.neighboringCountries |> Set.toList
                    )
                |> List.concat
    in
    List.member countryIdToAnnex playerNeighborCountries


getCountryStatus : String -> Player -> Dict.Dict Int Player -> CountryStatus
getCountryStatus countryId currentPlayer players =
    case Dict.get countryId currentPlayer.countries of
        Just playerCountry ->
            OccupiedByCurrentPlayer playerCountry

        Nothing ->
            case
                players
                    |> Dict.foldl
                        (\playerId player result ->
                            case result of
                                Just _ ->
                                    result

                                Nothing ->
                                    case Dict.get countryId player.countries of
                                        Just playerCountry ->
                                            Just (OccupiedByOpponent playerId player playerCountry)

                                        Nothing ->
                                            Nothing
                        )
                        Nothing
            of
                Just occupiedByOppenent ->
                    occupiedByOppenent

                Nothing ->
                    Unoccupied


nextPlayerTurn : Int -> String -> Dict.Dict Int Player -> PlayerTurn -> PlayerTurn
nextPlayerTurn totalPlayers countryId players (PlayerTurn currentPlayer playerTurnStage) =
    case playerTurnStage of
        TroopPlacement _ ->
            PlayerTurn currentPlayer AttackAnnexOrPort

        AttackAnnexOrPort ->
            let
                capitolsRemaining =
                    players
                        |> Dict.values
                        |> List.foldl
                            (\player capitols ->
                                case player.capitolStatus of
                                    Capitol capitolId _ ->
                                        capitolId :: capitols

                                    NoCapitol ->
                                        capitols
                            )
                            []
            in
            if List.length capitolsRemaining == 1 then
                PlayerTurn -1 (GameOver currentPlayer)

            else
                PlayerTurn currentPlayer TroopMovement

        TroopMovement ->
            PlayerTurn currentPlayer (TroopMovementFromSelected countryId)

        TroopMovementFromSelected _ ->
            PlayerTurn (nextPlayerCheckForDeadPlayers currentPlayer players) (TroopPlacement (numberOfTroopsToPlace (nextPlayerCheckForDeadPlayers currentPlayer players) players))

        CapitolPlacement ->
            let
                nextPlayerId =
                    remainderBy (Dict.size players) currentPlayer + 1
            in
            if currentPlayer == totalPlayers then
                PlayerTurn nextPlayerId (TroopPlacement (numberOfTroopsToPlace nextPlayerId players))

            else
                PlayerTurn nextPlayerId CapitolPlacement

        GameOver _ ->
            PlayerTurn -1 playerTurnStage


nextPlayerCheckForDeadPlayers : Int -> Dict.Dict Int Player -> Int
nextPlayerCheckForDeadPlayers currentPlayerId players =
    let
        nextPlayerId =
            remainderBy (Dict.size players) currentPlayerId + 1
    in
    case Dict.get nextPlayerId players of
        Just newCurrentPlayer ->
            case newCurrentPlayer.capitolStatus of
                Capitol _ _ ->
                    nextPlayerId

                NoCapitol ->
                    nextPlayerCheckForDeadPlayers nextPlayerId players

        Nothing ->
            currentPlayerId


numberOfTroopsToPlace : Int -> Dict.Dict Int Player -> Int
numberOfTroopsToPlace playerId players =
    case Dict.get playerId players of
        Just player ->
            Dict.size player.countries * troopsPerCountryPerTurn

        Nothing ->
            -1


type CountryStatus
    = Unoccupied
    | OccupiedByOpponent Int Player PlayerCountry
    | OccupiedByCurrentPlayer PlayerCountry



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        ConfiguringGame configuringGameSettings ->
            Element.layout [ Element.width Element.fill ]
                (Element.column
                    [ Element.width Element.fill, Element.centerX ]
                    [ Element.el [ Element.centerX ]
                        (viewConfiguration configuringGameSettings)
                    ]
                )

        LoadingGame _ _ ->
            Element.layout []
                (Element.column [ Element.width Element.fill, Element.height Element.fill ]
                    [(Element.image
                        [ Element.centerX, Element.centerY ]
                        { src = "/loading.gif", description = "Loading" }
                    ), Element.text "Loading"]
                )

        PlayingGame attributes ->
            Element.layout [ Element.width Element.fill ]
                (Element.row [ Element.centerX ]
                    [ viewSideBar attributes
                    , Element.column
                        [ Element.centerX ]
                        ([ Element.el [ Element.centerX, Element.width Element.fill ]
                            (renderMap attributes.players attributes.map
                                |> Element.html
                            )
                         ]
                            ++ (case attributes.error of
                                    Just error ->
                                        [ Element.text error ]

                                    Nothing ->
                                        []
                               )
                            ++ (case attributes.currentPlayerTurn of
                                    PlayerTurn playerId playerTurnStage ->
                                        case Dict.get playerId attributes.players of
                                            Just player ->
                                                [ viewPlayerTurnStatus playerId player playerTurnStage ]

                                            Nothing ->
                                                []
                                -- Should hopefully never happen
                               )
                        )
                    ]
                )


viewSideBar : PlayingGameAttributes -> Element.Element Msg
viewSideBar playingGameAttributes =
    case playingGameAttributes.currentPlayerTurn of
        PlayerTurn _ playerTurnStage ->
            Element.column [ Element.width (Element.px 200), Element.alignTop ]
                (if canPass playerTurnStage then
                    [ Element.Input.button
                        (defaultButtonAttributes
                            ++ [ Element.width (Element.px 100)
                               , Element.centerX
                               , Element.Background.color (Element.rgb255 0 100 100)
                               ]
                        )
                        { onPress = Just Pass, label = Element.text "Pass" }
                    ]

                 else
                    [ Element.el [ Element.height (Element.px 30) ] Element.none ]
                )


defaultButtonAttributes : List (Element.Attribute msg)
defaultButtonAttributes =
    [ Element.Border.solid
    , Element.Border.width 1
    , Element.Border.color (Element.rgb 0 0 0)
    , Element.padding 10
    ]


canPass : PlayerTurnStage -> Bool
canPass playerTurnStage =
    case playerTurnStage of
        TroopMovement ->
            True

        TroopMovementFromSelected _ ->
            True

        AttackAnnexOrPort ->
            True

        _ ->
            False


viewConfiguration : ConfigurationAttributes -> Element.Element Msg
viewConfiguration configurationAttributes =
    Element.row
        [ Element.width Element.fill ]
        [ Element.Input.text
            []
            { onChange = NumberOfPlayersChanged
            , text = configurationAttributes.numberOfPlayers
            , placeholder = Nothing
            , label = Element.Input.labelLeft [ Element.centerY ] (Element.text "Number of players")
            }
        , Element.Input.button (defaultButtonAttributes ++ [ Element.Background.color (Element.rgb255 0 150 0) ]) { onPress = Just StartGameClicked, label = Element.text "Start Game" }
        ]


viewPlayerTurnStatus : Int -> Player -> PlayerTurnStage -> Element.Element Msg
viewPlayerTurnStatus playerId player playerTurnStage =
    Element.el [ Element.width Element.fill, Element.Background.color (colorToElementColor player.color), Element.padding 5 ]
        (Element.text
            (case playerTurnStage of
                CapitolPlacement ->
                    "Player " ++ String.fromInt playerId ++ " is placing capitol"

                TroopPlacement numberOfTroops ->
                    "Player " ++ String.fromInt playerId ++ " is placing " ++ String.fromInt numberOfTroops ++ " troops"

                AttackAnnexOrPort ->
                    "Player " ++ String.fromInt playerId ++ " is attacking, annexing, or building a port"

                TroopMovement ->
                    "Player " ++ String.fromInt playerId ++ " is moving troops"

                TroopMovementFromSelected fromCountryId ->
                    "Player " ++ String.fromInt playerId ++ " is moving from " ++ fromCountryId

                GameOver winnerPlayerId ->
                    "Player " ++ String.fromInt winnerPlayerId ++ " wins!!!"
            )
        )


colorToElementColor : Color.Color -> Element.Color
colorToElementColor color =
    color |> Color.toRgba |> Element.fromRgb


type alias BorderSegment =
    ( ( Float, Float ), ( Float, Float ) )



-- Parsing
-- It's terrible, but it works. Eventually look into using a real parser.


parseRawMap : String -> RawGameMap
parseRawMap text =
    let
        rowStrings : List String
        rowStrings =
            String.split "\n" text
                |> List.foldl
                    (\row result ->
                        case result of
                            ( rawGameMap, rowIndex ) ->
                                if rowIndex then
                                    if row /= "{Country Names}" then
                                        ( row :: rawGameMap
                                        , True
                                        )

                                    else
                                        ( rawGameMap, False )

                                else if row == "{Map}" then
                                    ( rawGameMap, True )

                                else
                                    ( rawGameMap, False )
                    )
                    ( [], False )
                |> Tuple.first

        rowsAndColumns : List (List String)
        rowsAndColumns =
            rowStrings
                |> List.foldl
                    (\row result ->
                        (String.split "." row
                            |> List.reverse
                            |> List.drop 1
                            |> List.reverse
                        )
                            :: result
                    )
                    []
    in
    rowsAndColumns
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( rowIndex, splitRow ) result ->
                splitRow
                    |> List.indexedMap Tuple.pair
                    |> List.foldl
                        (\( columnIndex, areaId ) innerResult ->
                            Dict.insert ( columnIndex, rowIndex ) areaId innerResult
                        )
                        result
            )
            Dict.empty


getMapDimensions : RawGameMap -> ( Int, Int )
getMapDimensions map =
    map
        |> Dict.keys
        |> List.foldl
            (\( x, y ) ( width, height ) ->
                ( if x + 1 > width then
                    x + 1

                  else
                    width
                , if y + 1 > height then
                    y + 1

                  else
                    height
                )
            )
            ( 0, 0 )


parseMap : String -> GameMap
parseMap text =
    let
        map =
            parseRawMap text

        dimensions =
            getMapDimensions map

        gameMapWithoutPolygons =
            map
                |> Dict.foldl
                    (\coordinates areaId gameMap ->
                        if isCountry areaId then
                            let
                                country =
                                    case Dict.get areaId gameMap.countries of
                                        Just existingCountry ->
                                            existingCountry

                                        Nothing ->
                                            { neighboringCountries = Set.empty
                                            , neighboringBodiesOfWater = Set.empty
                                            , coordinates = Set.singleton coordinates
                                            , polygon = []
                                            , borderEdges = Set.empty
                                            , center = ( 0, 0 )
                                            }

                                updatedCountry =
                                    country
                                        |> updateCountry areaId coordinates dimensions map
                            in
                            { gameMap | countries = Dict.insert areaId updatedCountry gameMap.countries }

                        else
                            let
                                bodyOfWater =
                                    case Dict.get areaId gameMap.bodiesOfWater of
                                        Just existingBodyOfWater ->
                                            existingBodyOfWater

                                        Nothing ->
                                            { neighboringCountries = Set.empty
                                            , borderEdges = Set.empty
                                            , coordinates = Set.singleton coordinates
                                            , polygon = []
                                            }
                            in
                            { gameMap
                                | bodiesOfWater = Dict.insert areaId (updateBodyOfWater areaId bodyOfWater coordinates dimensions map) gameMap.bodiesOfWater
                            }
                    )
                    { countries = Dict.empty, bodiesOfWater = Dict.empty, dimensions = dimensions }
    in
    { countries =
        gameMapWithoutPolygons.countries
            |> Dict.map
                (\_ country ->
                    let
                        edges =
                            getEdgesForArea country.coordinates defaultScale
                    in
                    { country
                        | polygon = coordinatesToPolygon edges
                        , center = getMedianCoordinates country.coordinates
                        , borderEdges = edges
                    }
                )
    , bodiesOfWater =
        gameMapWithoutPolygons.bodiesOfWater
            |> Dict.map
                (\_ bodyOfWater ->
                    let
                        edges =
                            getEdgesForArea bodyOfWater.coordinates defaultScale
                    in
                    { bodyOfWater
                        | polygon = coordinatesToPolygon edges
                        , borderEdges = getEdgesForArea bodyOfWater.coordinates defaultScale
                    }
                )
    , dimensions =
        ( (gameMapWithoutPolygons.dimensions |> Tuple.first) * defaultScale |> toFloat
        , (gameMapWithoutPolygons.dimensions |> Tuple.second) * defaultScale |> toFloat
        )
    }


updateCountry : String -> ( Int, Int ) -> ( Int, Int ) -> RawGameMap -> Country -> Country
updateCountry countryId coordinates mapDimensions rawMap country =
    let
        ( neighboringCountries, neighboringBodiesOfWater ) =
            getNeighborCoordinates coordinates mapDimensions
                |> Set.foldl
                    (\neighborCoordinate ( countries, bodiesOfWater ) ->
                        case Dict.get neighborCoordinate rawMap of
                            Just neighborId ->
                                if neighborId /= countryId then
                                    if isCountry neighborId then
                                        ( Set.insert neighborId countries, bodiesOfWater )

                                    else
                                        ( countries, Set.insert neighborId bodiesOfWater )

                                else
                                    ( countries, bodiesOfWater )

                            Nothing ->
                                ( countries, bodiesOfWater )
                    )
                    ( Set.empty, Set.empty )
    in
    { country
        | neighboringCountries =
            Set.union neighboringCountries country.neighboringCountries
        , neighboringBodiesOfWater =
            Set.union neighboringBodiesOfWater country.neighboringBodiesOfWater
        , coordinates = Set.insert coordinates country.coordinates
    }


updateBodyOfWater : String -> BodyOfWater -> ( Int, Int ) -> ( Int, Int ) -> RawGameMap -> BodyOfWater
updateBodyOfWater bodyOfWaterId bodyOfWater coordinates mapDimensions rawMap =
    let
        neighboringCountries =
            getNeighborCoordinates coordinates mapDimensions
                |> Set.foldl
                    (\neighborCoordinate countries ->
                        case Dict.get neighborCoordinate rawMap of
                            Just neighborId ->
                                if neighborId /= bodyOfWaterId then
                                    if isCountry neighborId then
                                        Set.insert neighborId countries

                                    else
                                        countries

                                else
                                    countries

                            Nothing ->
                                countries
                    )
                    Set.empty
    in
    { bodyOfWater
        | neighboringCountries =
            Set.union neighboringCountries bodyOfWater.neighboringCountries
        , coordinates = Set.insert coordinates bodyOfWater.coordinates
    }


getNeighborCoordinates : ( Int, Int ) -> ( Int, Int ) -> Set.Set ( Int, Int )
getNeighborCoordinates ( x, y ) ( width, height ) =
    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
        |> List.foldl
            (\( xOffset, yOffset ) result ->
                let
                    neighborX =
                        x + xOffset

                    neighborY =
                        y + yOffset
                in
                if neighborX >= 0 && neighborX < width && neighborY >= 0 && neighborY < height then
                    Set.insert ( neighborX, neighborY ) result

                else
                    result
            )
            Set.empty


isCountry : String -> Bool
isCountry areaId =
    String.length areaId < 4



-- Rendering


type alias Area =
    Set.Set ( Int, Int )


renderMap : Dict.Dict Int Player -> GameMap -> Html Msg
renderMap players map =
    let
        countryCollages : List (Collage.Collage Msg)
        countryCollages =
            map.countries
                |> Dict.map
                    (\countryId country ->
                        case findCountryOwner players countryId of
                            Just ( _, player, playerCountry ) ->
                                renderCountry countryId country.polygon country.borderEdges country.center country.coordinates player.color playerCountry.population player.capitolStatus

                            Nothing ->
                                renderCountry countryId country.polygon country.borderEdges country.center country.coordinates Color.gray 0 NoCapitol
                    )
                |> Dict.values

        background =
            Collage.polygon
                [ ( 0, 0 )
                , ( 0, map.dimensions |> Tuple.second )
                , ( map.dimensions |> Tuple.first, map.dimensions |> Tuple.second )
                , ( map.dimensions |> Tuple.first, 0 )
                ]

        backgroundWater =
            background
                |> Collage.filled (Collage.uniform Color.blue)

        backgroundBorder =
            background
                |> Collage.outlined (Collage.solid (toFloat defaultScale / 8.0) (Collage.uniform Color.black))
    in
    Collage.group (countryCollages ++ [ backgroundBorder, backgroundWater ])
        |> Collage.Render.svg


isCountryIdCapitol : Player -> String -> Bool
isCountryIdCapitol player countryId =
    case player.capitolStatus of
        Capitol capitolId _ ->
            capitolId == countryId

        NoCapitol ->
            False


findCountryOwner : Dict.Dict Int Player -> String -> Maybe ( Int, Player, PlayerCountry )
findCountryOwner players countryId =
    players
        |> Dict.foldl
            (\playerId player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countries
                            |> Maybe.map (\playerCountry -> ( playerId, player, playerCountry ))
            )
            Nothing


renderCountry : String -> List ( Float, Float ) -> Set.Set ( ( Float, Float ), ( Float, Float ) ) -> ( Int, Int ) -> Area -> Color.Color -> Int -> CapitolStatus -> Collage.Collage Msg
renderCountry countryId polygon borderSegments medianCoordinates area color troopCount capitolStatus =
    Collage.group
        [ renderTroopCount medianCoordinates troopCount
        , renderArea polygon borderSegments color capitolStatus countryId
        ]
        |> Collage.Events.onClick (CountryClicked countryId)


renderTroopCount : ( Int, Int ) -> Int -> Collage.Collage msg
renderTroopCount ( medianX, medianY ) troopCount =
    if troopCount > 0 then
        troopCount
            |> String.fromInt
            |> Collage.Text.fromString
            |> Collage.Text.color Color.black
            |> Collage.Text.size (defaultScale * 100 // 120)
            |> Collage.rendered
            |> Collage.shift ( (toFloat medianX + 0.5) * toFloat defaultScale, (toFloat medianY + 0.5) * toFloat defaultScale )

    else
        Collage.group []


getMedianCoordinates : Area -> ( Int, Int )
getMedianCoordinates area =
    area
        |> Set.foldl
            (\( x, y ) ( xs, ys ) ->
                ( x :: xs, y :: ys )
            )
            ( [], [] )
        |> Tuple.mapBoth List.sort List.sort
        |> Tuple.mapBoth
            (\xs ->
                xs
                    |> List.drop (Set.size area // 2)
                    |> List.head
                    |> Maybe.withDefault 0
            )
            (\ys ->
                ys
                    |> List.drop (Set.size area // 2)
                    |> List.head
                    |> Maybe.withDefault 0
            )


renderArea : List ( Float, Float ) -> Set.Set BorderSegment -> Color.Color -> CapitolStatus -> String -> Collage.Collage msg
renderArea polygonPoints segments color capitolStatus countryId =
    let
        scale =
            defaultScale

        ( capitolDot, capitolDotsCoords ) =
            case capitolStatus of
                Capitol capitolId coords ->
                    if countryId == capitolId then
                        ( [ Collage.square (toFloat scale / 10.0)
                                |> Collage.filled (Collage.uniform Color.black)
                          ]
                        , coords
                        )

                    else
                        ( [], Set.empty )

                NoCapitol ->
                    ( [], Set.empty )

        renderedDot =
            capitolDot
                |> Collage.group

        polygon =
            Collage.polygon polygonPoints

        polygonBorder =
            polygon
                |> Collage.outlined (Collage.solid (toFloat defaultScale / 8.0) (Collage.uniform Color.black))

        polygonFill =
            polygon
                |> Collage.filled (Collage.uniform color)

        drawnDots =
            capitolDotsCoords
                |> Set.foldl
                    (\coordinates result ->
                        (renderedDot |> Collage.shift coordinates) :: result
                    )
                    []
    in
    Collage.group (drawnDots ++ [ polygonBorder, polygonFill ])



-- Collage.group (drawnDots ++ borderSegments ++ [ polygonFill ])


capitolDotsCoordinates : Area -> Int -> Set.Set ( Float, Float )
capitolDotsCoordinates area scale =
    area
        |> Set.map
            (\( x, y ) ->
                ( (toFloat x + 0.5) * toFloat scale, (toFloat y + 0.5) * toFloat scale )
            )


coordinatesToPolygon : Set.Set ( ( Float, Float ), ( Float, Float ) ) -> List ( Float, Float )
coordinatesToPolygon edges =
    case edges |> Set.toList of
        ( point1, point2 ) :: _ ->
            recursiveStuff (Set.remove ( point1, point2 ) edges) point2 []

        _ ->
            []


recursiveStuff : Set.Set BorderSegment -> ( Float, Float ) -> List ( Float, Float ) -> List ( Float, Float )
recursiveStuff borderSegments currentPoint result =
    let
        maybeSegment =
            borderSegments
                |> Set.filter
                    (\( point1, point2 ) -> point1 == currentPoint || point2 == currentPoint)
                |> Set.toList
                |> List.head
    in
    case maybeSegment of
        Just ( point1, point2 ) ->
            let
                remainingSegments =
                    borderSegments
                        |> Set.remove ( point1, point2 )
            in
            recursiveStuff remainingSegments
                (if currentPoint == point1 then
                    point2

                 else
                    point1
                )
                (currentPoint :: result)

        Nothing ->
            currentPoint :: result



--should never happen
-- Should never happen


getEdgesForArea : Area -> Int -> Set.Set BorderSegment
getEdgesForArea area scale =
    area
        |> Set.foldl
            (\coordinate result ->
                Set.union result (getEdgesForCountryForCoordinate area coordinate scale)
            )
            Set.empty


scaleCoordinate : Int -> ( Int, Int ) -> ( Float, Float )
scaleCoordinate scale ( x, y ) =
    ( x * scale |> toFloat, y * scale |> toFloat )


scaleEdge : Int -> ( ( Int, Int ), ( Int, Int ) ) -> BorderSegment
scaleEdge scale ( point1, point2 ) =
    ( scaleCoordinate scale point1, scaleCoordinate scale point2 )


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Int -> Set.Set BorderSegment
getEdgesForCountryForCoordinate allAreas ( x, y ) scaleFactor =
    let
        left =
            ( x - 1, y )

        leftEdge =
            ( ( x, y ), ( x, y + 1 ) )

        right =
            ( x + 1, y )

        rightEdge =
            ( ( x + 1, y ), ( x + 1, y + 1 ) )

        above =
            ( x, y - 1 )

        aboveEdge =
            ( ( x, y ), ( x + 1, y ) )

        below =
            ( x, y + 1 )

        belowEdge =
            ( ( x, y + 1 ), ( x + 1, y + 1 ) )

        adjacentEdges =
            [ ( left, leftEdge )
            , ( right, rightEdge )
            , ( above, aboveEdge )
            , ( below, belowEdge )
            ]
    in
    adjacentEdges
        |> List.foldl
            (\( adjacent, edge ) result ->
                if Set.member adjacent allAreas then
                    result

                else
                    Set.insert (scaleEdge scaleFactor edge) result
            )
            Set.empty


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LoadingGame _ _ ->
            Time.every 1000 (always LoadGame)

        _ ->
            Sub.none
