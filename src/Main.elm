module Main exposing (BorderSegment(..), Country, getEdgesForArea, getMapDimensions, getNeighborCoordinates, main, parseMap, parseRawMap, updateCountry)

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
import Maps.SuperSimple
import Set



-- Settings


troopsPerCountryPerTurn : Int
troopsPerCountryPerTurn =
    1


defaultScale : Int
defaultScale =
    12


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
    }


type alias Country =
    { coordinates : Set.Set ( Int, Int )
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias BodyOfWater =
    { coordinates : Set.Set ( Int, Int )
    , neighboringCountries : Set.Set String
    }


type alias RawGameMap =
    Dict.Dict ( Int, Int ) String


type alias PlayerCountry =
    { population : Int
    }


type Model
    = ConfiguringGame ConfigurationAttributes
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
    , capitolId : Maybe String
    , color : Color.Color
    }


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame configurationOptions ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { configurationOptions | numberOfPlayers = numberOfPlayers }, Cmd.none )

                StartGameClicked ->
                    let
                        numberOfPlayers =
                            configurationOptions.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    ( PlayingGame
                        { map = parseMap Maps.Big.map
                        , players =
                            List.range 1 numberOfPlayers
                                |> List.map
                                    (\playerId ->
                                        ( playerId
                                        , { countries = Dict.empty
                                          , name = "Player " ++ String.fromInt playerId
                                          , capitolId = Nothing
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

                Pass ->
                    ( model, Cmd.none )

                CountryClicked _ ->
                    ( model, Cmd.none )

        PlayingGame attributes ->
            case msg of
                CountryClicked id ->
                    case Dict.get id attributes.map.countries of
                        Just country ->
                            case attributes.currentPlayerTurn of
                                PlayerTurn playerId playerTurnStage ->
                                    case Dict.get playerId attributes.players of
                                        Just currentPlayer ->
                                            ( PlayingGame (handleCountryClickFromPlayer id country attributes)
                                            , Cmd.none
                                            )

                                        Nothing ->
                                            ( PlayingGame attributes, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Pass ->
                    case attributes.currentPlayerTurn of
                        PlayerTurn playerId (TroopMovementFromSelected _) ->
                            ( PlayingGame
                                { attributes
                                    | currentPlayerTurn =
                                        nextPlayerTurn attributes.numberOfPlayers "-1" attributes.players attributes.currentPlayerTurn

                                    -- Gross
                                    , error = Nothing
                                }
                            , Cmd.none
                            )

                        PlayerTurn playerId TroopMovement ->
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

                        PlayerTurn playerId AttackAnnexOrPort ->
                            ( PlayingGame
                                { attributes
                                    | currentPlayerTurn = nextPlayerTurn attributes.numberOfPlayers "-1" attributes.players attributes.currentPlayerTurn -- Gross
                                    , error = Nothing
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                NumberOfPlayersChanged numberOfPlayers ->
                    ( model, Cmd.none )

                StartGameClicked ->
                    ( model, Cmd.none )


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
                                                , capitolId = Just clickedCountryId
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
                                                if remainingTroops >= 0 then
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
                                                            , capitolId = Nothing
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
                                    { model | currentPlayerTurn = nextPlayerTurn model.numberOfPlayers clickedCountryId model.players model.currentPlayerTurn }

                                _ ->
                                    { model | error = Just "You must move troops from your own country" }

                        TroopMovementFromSelected fromCountryId ->
                            case ( getCountryStatus fromCountryId currentPlayer model.players, getCountryStatus clickedCountryId currentPlayer model.players ) of
                                ( OccupiedByCurrentPlayer playerCountryFrom, OccupiedByCurrentPlayer playerCountryTo ) ->
                                    if canMoveTroops model.map fromCountryId clickedCountryId then
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


canMoveTroops : GameMap -> String -> String -> Bool
canMoveTroops gameMap fromCountryId toCountryId =
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
                                case player.capitolId of
                                    Just capitolId ->
                                        capitolId :: capitols

                                    Nothing ->
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
            case newCurrentPlayer.capitolId of
                Just _ ->
                    nextPlayerId

                Nothing ->
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

        PlayingGame attributes ->
            Element.layout [ Element.width Element.fill ]
                (Element.row []
                    [ viewSideBar attributes
                    , Element.column
                        [ Element.centerX ]
                        ([ Element.el [ Element.centerX ]
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
                        (defaultButtonAttributes ++ [ 
                         Element.width (Element.px 100)
                        , Element.centerX
                        , Element.Background.color (Element.rgb255 0 100 100)
                        ])
                        { onPress = Just Pass, label = Element.text "Pass" }
                    ]

                 else
                    []
                )


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
            , label = Element.Input.labelLeft [Element.centerY] (Element.text "Number of players")
            }
        , Element.Input.button (defaultButtonAttributes ++ [Element.Background.color (Element.rgb255 0 150 0)]) { onPress = Just StartGameClicked, label = Element.text "Start Game" }
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


type BorderSegment
    = BorderSegment ( Float, Float ) ( Float, Float )



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
                    width
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
    in
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
                                    }
                    in
                    { gameMap
                        | countries = Dict.insert areaId (updateCountry areaId country coordinates dimensions map) gameMap.countries
                    }

                else
                    let
                        bodyOfWater =
                            case Dict.get areaId gameMap.bodiesOfWater of
                                Just existingBodyOfWater ->
                                    existingBodyOfWater

                                Nothing ->
                                    { neighboringCountries = Set.empty
                                    , coordinates = Set.singleton coordinates
                                    }
                    in
                    { gameMap
                        | bodiesOfWater = Dict.insert areaId (updateBodyOfWater areaId bodyOfWater coordinates dimensions map) gameMap.bodiesOfWater
                    }
            )
            { countries = Dict.empty, bodiesOfWater = Dict.empty }


updateCountry : String -> Country -> ( Int, Int ) -> ( Int, Int ) -> RawGameMap -> Country
updateCountry countryId country coordinates mapDimensions rawMap =
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
                            Just ( playerId, player, playerCountry ) ->
                                let
                                    isCapitol =
                                        isCountryIdCapitol player countryId
                                in
                                case playerId of
                                    1 ->
                                        renderCountry countryId country.coordinates Color.lightRed playerCountry.population isCapitol

                                    2 ->
                                        renderCountry countryId country.coordinates Color.lightPurple playerCountry.population isCapitol

                                    3 ->
                                        renderCountry countryId country.coordinates Color.lightYellow playerCountry.population isCapitol

                                    4 ->
                                        renderCountry countryId country.coordinates Color.lightGreen playerCountry.population isCapitol

                                    5 ->
                                        renderCountry countryId country.coordinates Color.lightOrange playerCountry.population isCapitol

                                    6 ->
                                        renderCountry countryId country.coordinates Color.brown playerCountry.population isCapitol

                                    _ ->
                                        renderCountry countryId country.coordinates Color.black playerCountry.population isCapitol

                            Nothing ->
                                renderCountry countryId country.coordinates Color.gray 0 False
                    )
                |> Dict.values

        waterCollages =
            map.bodiesOfWater
                |> Dict.values
                |> List.map
                    (\bodyOfWater -> renderArea bodyOfWater.coordinates Color.blue False)
    in
    Collage.group (countryCollages ++ waterCollages)
        |> Collage.Render.svg


isCountryIdCapitol : Player -> String -> Bool
isCountryIdCapitol player countryId =
    case player.capitolId of
        Just capitolId ->
            capitolId == countryId

        Nothing ->
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


renderCountry : String -> Area -> Color.Color -> Int -> Bool -> Collage.Collage Msg
renderCountry countryId area color troopCount isCapitol =
    Collage.group
        [ renderTroopCount area troopCount
        , renderArea area color isCapitol
        ]
        |> Collage.Events.onClick (CountryClicked countryId)


renderTroopCount : Area -> Int -> Collage.Collage msg
renderTroopCount area troopCount =
    let
        ( medianX, medianY ) =
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

        troopCountDisplay =
            if troopCount > 0 then
                String.fromInt troopCount

            else
                ""
    in
    Collage.Text.fromString troopCountDisplay
        |> Collage.Text.color Color.black
        |> Collage.Text.size (defaultScale * 100 // 120)
        |> Collage.rendered
        |> Collage.shift ( (toFloat medianX + 0.5) * toFloat defaultScale, (toFloat medianY + 0.5) * toFloat defaultScale )


renderArea : Area -> Color.Color -> Bool -> Collage.Collage msg
renderArea area color isCapitol =
    let
        segments =
            getEdgesForArea area defaultScale
                |> List.map (\(BorderSegment p1 p2) -> Collage.segment p1 p2)

        blocks =
            getBlocksForArea area defaultScale color isCapitol

        borderSegments =
            List.map
                (\segment ->
                    Collage.traced Collage.defaultLineStyle segment
                )
                segments
    in
    Collage.group (borderSegments ++ blocks)


getEdgesForArea : Area -> Int -> List BorderSegment
getEdgesForArea area scale =
    area
        |> Set.foldl
            (\coordinate result ->
                result ++ getEdgesForCountryForCoordinate area coordinate scale
            )
            []


getBlocksForArea : Area -> Int -> Color.Color -> Bool -> List (Collage.Collage msg)
getBlocksForArea area scale color isCapitol =
    let
        capitolDot =
            if isCapitol then
                [ Collage.square (toFloat scale / 10.0)
                    |> Collage.filled (Collage.uniform Color.black)
                ]

            else
                []

        block =
            Collage.group
                (capitolDot
                    ++ [ Collage.square (toFloat scale)
                            |> Collage.filled (Collage.uniform color)
                       ]
                )
    in
    area
        |> Set.foldl
            (\( x, y ) result ->
                (block |> Collage.shift ( (toFloat x + 0.5) * toFloat scale, (toFloat y + 0.5) * toFloat scale )) :: result
            )
            []


scaleCoordinate : Int -> ( Int, Int ) -> ( Float, Float )
scaleCoordinate scale ( x, y ) =
    ( x * scale |> toFloat, y * scale |> toFloat )


scaleEdge : Int -> ( ( Int, Int ), ( Int, Int ) ) -> BorderSegment
scaleEdge scale ( point1, point2 ) =
    BorderSegment (scaleCoordinate scale point1) (scaleCoordinate scale point2)


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Int -> List BorderSegment
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
                    scaleEdge scaleFactor edge :: result
            )
            []
