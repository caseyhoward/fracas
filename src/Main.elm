module Main exposing (BorderSegment, CapitolStatus(..), Country, coordinatesToPolygon, getEdgesForArea, getMapDimensions, getNeighborCoordinates, main, parseMap, parseRawMap, removePlayerCountry, updateCountry)

import Array
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
import Random
import Random.Dict
import Random.List
import Set
import Time



-- Settings


troopsPerCountryPerTurn : Int
troopsPerCountryPerTurn =
    1


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    20


defaultScale : Int
defaultScale =
    12


countryBorderColor : Color.Color
countryBorderColor =
    -- Color.rgb255 64 64 64
    Color.black


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
    { coordinates : Set.Set ( Int, Int ) -- Only needed for making the capitol dots
    , polygon : List ( Float, Float )
    , center : ( Int, Int )
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias BodyOfWater =
    { neighboringCountries : Set.Set String
    }


type alias RawGameMap =
    Dict.Dict ( Int, Int ) String


type Model
    = ConfiguringGame ConfigurationAttributes
    | LoadingGame Int ConfigurationAttributes
    | PlayingGame PlayingGameAttributes
    | GeneratingRandomTroopCounts ConfigurationAttributes


type alias ConfigurationAttributes =
    { numberOfPlayers : String }


type alias PlayingGameAttributes =
    { currentPlayerTurn : PlayerTurn
    , map : GameMap
    , players : Dict.Dict Int Player
    , neutralCountryTroops : Dict.Dict String Int
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
    , countries : Dict.Dict String Int
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
    | NeutralCountryTroopCountsGenerated (Dict.Dict String Int)


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

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

        LoadingGame counter configurationOptions ->
            case msg of
                LoadGame ->
                    if counter > 0 then
                        let
                            map =
                                parseMap Maps.Big.map
                        in
                        ( GeneratingRandomTroopCounts configurationOptions
                        , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys map.countries))
                        )

                    else
                        ( LoadingGame (counter + 1) configurationOptions, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GeneratingRandomTroopCounts configurationOptions ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    let
                        numberOfPlayers =
                            configurationOptions.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    let
                        map =
                            parseMap Maps.Big.map
                    in
                    ( PlayingGame
                        { map = map
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
                        , neutralCountryTroops = neutralCountryTroopCounts
                        }
                    , Cmd.none
                    )

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

                                    , error = Nothing
                                }
                            , Cmd.none
                            )

                        PlayerTurn _ TroopMovement ->
                            ( PlayingGame
                                { attributes
                                    | currentPlayerTurn =
                                        attributes.currentPlayerTurn
                                            |> nextPlayerTurn attributes.numberOfPlayers "-1" attributes.players
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

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )


getDefaultColor : Int -> Color.Color
getDefaultColor playerId =
    case Dict.get playerId defaultPlayerColors of
        Just color ->
            color

        Nothing ->
            Color.black


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String Int)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        100
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (Random.int 1 maximumNeutralCountryTroops)


handleCountryClickFromPlayer : String -> Country -> PlayingGameAttributes -> PlayingGameAttributes
handleCountryClickFromPlayer clickedCountryId country playingGameAttributes =
    case playingGameAttributes.currentPlayerTurn of
        PlayerTurn currentPlayerId playerTurnStage ->
            case Dict.get currentPlayerId playingGameAttributes.players of
                Just currentPlayer ->
                    case playerTurnStage of
                        CapitolPlacement ->
                            attemptToPlaceCapitol clickedCountryId currentPlayerId playingGameAttributes

                        TroopPlacement numberOfTroops ->
                            case getCountryStatus clickedCountryId currentPlayer playingGameAttributes.players of
                                OccupiedByCurrentPlayer troopCount ->
                                    let
                                        updatedPlayer =
                                            { currentPlayer
                                                | countries =
                                                    Dict.insert clickedCountryId (troopCount + numberOfTroops) currentPlayer.countries
                                            }

                                        updatedPlayers =
                                            Dict.insert currentPlayerId updatedPlayer playingGameAttributes.players
                                    in
                                    { playingGameAttributes
                                        | players = updatedPlayers
                                        , currentPlayerTurn = nextPlayerTurn playingGameAttributes.numberOfPlayers clickedCountryId updatedPlayers playingGameAttributes.currentPlayerTurn
                                        , error = Nothing
                                    }

                                OccupiedByOpponent _ _ _ ->
                                    { playingGameAttributes | error = Just "You must put troops in your own country" }

                                Unoccupied ->
                                    { playingGameAttributes | error = Just "You must put troops in your own country" }

                        AttackAnnexOrPort ->
                            attackAnnexOrPort clickedCountryId country currentPlayerId currentPlayer playingGameAttributes

                        TroopMovement ->
                            case getCountryStatus clickedCountryId currentPlayer playingGameAttributes.players of
                                OccupiedByCurrentPlayer _ ->
                                    case Dict.get clickedCountryId currentPlayer.countries of
                                        Just troopCount ->
                                            if troopCount > 0 then
                                                { playingGameAttributes
                                                    | currentPlayerTurn = nextPlayerTurn playingGameAttributes.numberOfPlayers clickedCountryId playingGameAttributes.players playingGameAttributes.currentPlayerTurn
                                                    , error = Nothing
                                                }

                                            else
                                                { playingGameAttributes | error = Just "Select a country with troops" }

                                        Nothing ->
                                            { playingGameAttributes | error = Just "This shouldn't happen" }

                                _ ->
                                    { playingGameAttributes | error = Just "You must move troops from your own country" }

                        TroopMovementFromSelected fromCountryId ->
                            case ( getCountryStatus fromCountryId currentPlayer playingGameAttributes.players, getCountryStatus clickedCountryId currentPlayer playingGameAttributes.players ) of
                                ( OccupiedByCurrentPlayer playerCountryFromTroopCount, OccupiedByCurrentPlayer playerCountryToTroopCount ) ->
                                    if isCountryReachableFromOtherCountry playingGameAttributes.map fromCountryId clickedCountryId then
                                        let
                                            updatedPlayer =
                                                { currentPlayer
                                                    | countries =
                                                        currentPlayer.countries
                                                            |> Dict.insert fromCountryId 0
                                                            |> Dict.insert
                                                                clickedCountryId
                                                                (playerCountryFromTroopCount + playerCountryToTroopCount)
                                                }

                                            updatedPlayers =
                                                Dict.insert currentPlayerId updatedPlayer playingGameAttributes.players
                                        in
                                        { playingGameAttributes
                                            | players = updatedPlayers
                                            , currentPlayerTurn = nextPlayerTurn playingGameAttributes.numberOfPlayers clickedCountryId updatedPlayers playingGameAttributes.currentPlayerTurn
                                            , error = Nothing
                                        }

                                    else
                                        { playingGameAttributes | error = Just "You can't move troops between those countries" }

                                _ ->
                                    { playingGameAttributes | error = Just "You must move troops to your own country" }

                        GameOver _ ->
                            { playingGameAttributes | error = Nothing }

                Nothing ->
                    playingGameAttributes


attemptToPlaceCapitol : String -> Int -> PlayingGameAttributes -> PlayingGameAttributes
attemptToPlaceCapitol clickedCountryId currentPlayerId playingGameAttributes =
    case ( Dict.get currentPlayerId playingGameAttributes.players, Dict.get clickedCountryId playingGameAttributes.map.countries ) of
        ( Just currentPlayer, Just clickedCountry ) ->
            case getCountryStatus clickedCountryId currentPlayer playingGameAttributes.players of
                OccupiedByCurrentPlayer _ ->
                    { playingGameAttributes | error = Just "Error: Somehow you are placing a second capitol" }

                OccupiedByOpponent _ _ _ ->
                    { playingGameAttributes | error = Just "You must select an unoccuppied country" }

                Unoccupied ->
                    let
                        neutralTroopCount =
                            Dict.get clickedCountryId playingGameAttributes.neutralCountryTroops |> Maybe.withDefault 0

                        updatedPlayer =
                            { currentPlayer
                                | countries =
                                    Dict.insert clickedCountryId neutralTroopCount currentPlayer.countries
                                , capitolStatus = Capitol clickedCountryId (capitolDotsCoordinates clickedCountry.coordinates defaultScale)
                            }
                    in
                    { playingGameAttributes
                        | players = Dict.insert currentPlayerId updatedPlayer playingGameAttributes.players
                        , neutralCountryTroops = Dict.remove clickedCountryId playingGameAttributes.neutralCountryTroops
                        , currentPlayerTurn = nextPlayerTurn playingGameAttributes.numberOfPlayers clickedCountryId playingGameAttributes.players playingGameAttributes.currentPlayerTurn
                        , error = Nothing
                    }

        _ ->
            { playingGameAttributes
                | error = Just "Something bad happened"
            }


isCountryReachableFromOtherCountry : GameMap -> String -> String -> Bool
isCountryReachableFromOtherCountry gameMap fromCountryId toCountryId =
    -- Used for moving troops and seeing if a country is annexable. This will also need to take ports into account when those are added.
    case Dict.get fromCountryId gameMap.countries of
        Just fromCountry ->
            Set.member toCountryId fromCountry.neighboringCountries

        Nothing ->
            False


attackAnnexOrPort : String -> Country -> Int -> Player -> PlayingGameAttributes -> PlayingGameAttributes
attackAnnexOrPort clickedCountryId clickedCountry currentPlayerId currentPlayer playingGameAttributes =
    case getCountryStatus clickedCountryId currentPlayer playingGameAttributes.players of
        OccupiedByCurrentPlayer _ ->
            { playingGameAttributes
                | error = Just "TODO: Implement building port"
            }

        OccupiedByOpponent opponentPlayerId opponentPlayer opponentPlayerCountry ->
            attemptToAttackCountry currentPlayerId currentPlayer opponentPlayerId opponentPlayer opponentPlayerCountry clickedCountryId clickedCountry playingGameAttributes

        Unoccupied ->
            attemptToAnnexCountry currentPlayerId currentPlayer clickedCountryId playingGameAttributes


attemptToAnnexCountry : Int -> Player -> String -> PlayingGameAttributes -> PlayingGameAttributes
attemptToAnnexCountry currentPlayerId currentPlayer clickedCountryId playingGameAttributes =
    if canAnnexCountry playingGameAttributes.map currentPlayer clickedCountryId then
        let
            neutralTroopCount =
                Dict.get clickedCountryId playingGameAttributes.neutralCountryTroops |> Maybe.withDefault 0

            updatedPlayer =
                { currentPlayer
                    | countries =
                        Dict.insert clickedCountryId neutralTroopCount currentPlayer.countries
                }
        in
        { playingGameAttributes
            | players = Dict.insert currentPlayerId updatedPlayer playingGameAttributes.players
            , currentPlayerTurn = nextPlayerTurn playingGameAttributes.numberOfPlayers clickedCountryId playingGameAttributes.players playingGameAttributes.currentPlayerTurn
            , neutralCountryTroops = Dict.remove clickedCountryId playingGameAttributes.neutralCountryTroops
            , error = Nothing
        }

    else
        { playingGameAttributes
            | error = Just "You can't annex that country"
        }


type AttackResult
    = CurrentPlayerAcquiresOpponentCountry
    | OpponentCountryLosesTroops Int
    | OpponentEliminated
    | NotEnoughTroopsToAttack Int Int


attackResult : Player -> Int -> Player -> Int -> String -> Country -> PlayingGameAttributes -> AttackResult
attackResult currentPlayer opponentPlayerId opponentPlayer troopCount clickedCountryId clickedCountry playingGameAttributes =
    let
        ( attackStrength, defenseStrength ) =
            attackAndDefenseStrength clickedCountry.neighboringCountries currentPlayer playingGameAttributes.players opponentPlayerId troopCount

        remainingTroops =
            troopCount + defenseStrength - attackStrength
    in
    if attackStrength > defenseStrength then
        if remainingTroops > 0 then
            OpponentCountryLosesTroops remainingTroops

        else if isCountryIdCapitol opponentPlayer clickedCountryId then
            OpponentEliminated

        else
            CurrentPlayerAcquiresOpponentCountry

    else
        NotEnoughTroopsToAttack attackStrength defenseStrength


attemptToAttackCountry : Int -> Player -> Int -> Player -> Int -> String -> Country -> PlayingGameAttributes -> PlayingGameAttributes
attemptToAttackCountry currentPlayerId currentPlayer opponentPlayerId opponentPlayer troopCount clickedCountryId clickedCountry playingGameAttributes =
    case attackResult currentPlayer opponentPlayerId opponentPlayer troopCount clickedCountryId clickedCountry playingGameAttributes of
        OpponentCountryLosesTroops remainingTroops ->
            let
                updatedPlayers =
                    playingGameAttributes.players
                        |> updatePlayerTroopCountForCountry clickedCountryId opponentPlayerId remainingTroops
            in
            updateForSuccessfulAttack updatedPlayers playingGameAttributes

        OpponentEliminated ->
            let
                updatedPlayers =
                    playingGameAttributes.players
                        |> takeCountryFromOpponent clickedCountryId currentPlayerId opponentPlayerId
                        |> destroyPlayer opponentPlayerId
            in
            updateForSuccessfulAttack updatedPlayers playingGameAttributes

        CurrentPlayerAcquiresOpponentCountry ->
            let
                updatedPlayers =
                    playingGameAttributes.players
                        |> takeCountryFromOpponent clickedCountryId currentPlayerId opponentPlayerId
            in
            updateForSuccessfulAttack updatedPlayers playingGameAttributes

        NotEnoughTroopsToAttack attackStrength defenseStrength ->
            { playingGameAttributes
                | error = Just ("Not enough to attack: attack strength = " ++ String.fromInt attackStrength ++ ", defense strength = " ++ String.fromInt defenseStrength)
            }


updateForSuccessfulAttack : Dict.Dict Int Player -> PlayingGameAttributes -> PlayingGameAttributes
updateForSuccessfulAttack players playingGameAttributes =
    { playingGameAttributes
        | players = players
        , currentPlayerTurn = nextPlayerTurn playingGameAttributes.numberOfPlayers "-1" players playingGameAttributes.currentPlayerTurn
        , error = Nothing
    }


takeCountryFromOpponent : String -> Int -> Int -> Dict.Dict Int Player -> Dict.Dict Int Player
takeCountryFromOpponent countryId currentPlayerId opponentPlayerId players =
    players
        |> removePlayerCountry countryId opponentPlayerId
        |> updatePlayerTroopCountForCountry countryId currentPlayerId 0


updatePlayerTroopCountForCountry : String -> Int -> Int -> Dict.Dict Int Player -> Dict.Dict Int Player
updatePlayerTroopCountForCountry countryId playerId troops players =
    -- Make this return result with error if dict lookup fails
    case Dict.get playerId players of
        Just player ->
            players
                |> Dict.insert
                    playerId
                    { player
                        | countries =
                            player.countries
                                |> Dict.insert countryId troops
                    }

        Nothing ->
            players


removePlayerCountry : String -> Int -> Dict.Dict Int Player -> Dict.Dict Int Player
removePlayerCountry countryId playerId players =
    -- Make this return result with error if dict lookup fails
    case Dict.get playerId players of
        Just player ->
            players
                |> Dict.insert
                    playerId
                    { player
                        | countries = player.countries |> Dict.remove countryId
                    }

        Nothing ->
            players


destroyPlayer : Int -> Dict.Dict Int Player -> Dict.Dict Int Player
destroyPlayer playerId players =
    -- Make this return result with error if dict lookup fails
    case Dict.get playerId players of
        Just player ->
            players
                |> Dict.insert playerId { player | capitolStatus = NoCapitol, countries = Dict.empty }

        Nothing ->
            players


attackAndDefenseStrength : Set.Set String -> Player -> Dict.Dict Int Player -> Int -> Int -> ( Int, Int )
attackAndDefenseStrength countryBeingAttackedNeighboringCountries currentPlayer players opponentPlayerId opponentCountryTroopCount =
    countryBeingAttackedNeighboringCountries
        |> Set.foldl
            (\neighboringCountryId ( attack, defense ) ->
                case getCountryStatus neighboringCountryId currentPlayer players of
                    OccupiedByCurrentPlayer neighboringPlayerCountryTroopCount ->
                        ( attack + neighboringPlayerCountryTroopCount, defense )

                    OccupiedByOpponent neigborPlayerId _ neighboringPlayerCountryTroopCount ->
                        if neigborPlayerId == opponentPlayerId then
                            ( attack, defense + neighboringPlayerCountryTroopCount )

                        else
                            ( attack, defense )

                    _ ->
                        ( attack, defense )
            )
            ( 0, opponentCountryTroopCount )


canAnnexCountry : GameMap -> Player -> String -> Bool
canAnnexCountry gameMap player countryIdToAnnex =
    -- We already know the country is unoccuppied from an earlier check so just make sure it is reachable from one of the current players countries
    player.countries
        |> Dict.foldl
            (\playerCountryId _ isReachable ->
                isReachable || isCountryReachableFromOtherCountry gameMap playerCountryId countryIdToAnnex
            )
            False


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
nextPlayerTurn totalPlayers countryId players (PlayerTurn currentPlayerId playerTurnStage) =
    case playerTurnStage of
        TroopPlacement _ ->
            PlayerTurn currentPlayerId AttackAnnexOrPort

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
                PlayerTurn currentPlayerId (GameOver currentPlayerId)

            else
                PlayerTurn currentPlayerId TroopMovement

        TroopMovement ->
            PlayerTurn currentPlayerId (TroopMovementFromSelected countryId)

        TroopMovementFromSelected _ ->
            PlayerTurn (nextPlayerCheckForDeadPlayers currentPlayerId players) (TroopPlacement (numberOfTroopsToPlace (nextPlayerCheckForDeadPlayers currentPlayerId players) players))

        CapitolPlacement ->
            let
                nextPlayerId =
                    remainderBy (Dict.size players) currentPlayerId + 1
            in
            if currentPlayerId == totalPlayers then
                PlayerTurn nextPlayerId (TroopPlacement (numberOfTroopsToPlace nextPlayerId players))

            else
                PlayerTurn nextPlayerId CapitolPlacement

        GameOver _ ->
            -- This really should never happen since there shuoldn't be another turn after the game is over
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
    | OccupiedByOpponent Int Player Int
    | OccupiedByCurrentPlayer Int



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
                    [ Element.image
                        [ Element.centerX, Element.centerY ]
                        { src = "/loading.gif", description = "Loading" }
                    , Element.text "Loading"
                    ]
                )

        GeneratingRandomTroopCounts _ ->
            Element.layout [] Element.none

        PlayingGame attributes ->
            Element.layout [ Element.width Element.fill ]
                (Element.row [ Element.centerX ]
                    [ viewSideBar attributes
                    , Element.column
                        [ Element.centerX ]
                        ([ Element.el [ Element.centerX, Element.width Element.fill ]
                            (renderMap attributes.players attributes.neutralCountryTroops attributes.map |> Element.html)
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



-- viewConfigureTroopCount : [Element.Element Msg]
-- viewConfigureTroopCount =


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



-- Rendering


type alias Area =
    Set.Set ( Int, Int )


renderMap : Dict.Dict Int Player -> Dict.Dict String Int -> GameMap -> Html Msg
renderMap players neutralTroopCounts map =
    let
        countryCollages : List (Collage.Collage Msg)
        countryCollages =
            map.countries
                |> Dict.map
                    (\countryId country ->
                        case findCountryOwner players countryId of
                            Just ( _, player, troopCount ) ->
                                renderCountry countryId country.polygon country.center player.color troopCount player.capitolStatus

                            Nothing ->
                                renderCountry
                                    countryId
                                    country.polygon
                                    country.center
                                    Color.gray
                                    (Dict.get countryId neutralTroopCounts
                                        |> Maybe.withDefault 0
                                    )
                                    NoCapitol
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


findCountryOwner : Dict.Dict Int Player -> String -> Maybe ( Int, Player, Int )
findCountryOwner players countryId =
    players
        |> Dict.foldl
            (\playerId player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countries
                            |> Maybe.map (\troopCount -> ( playerId, player, troopCount ))
            )
            Nothing


renderCountry : String -> List ( Float, Float ) -> ( Int, Int ) -> Color.Color -> Int -> CapitolStatus -> Collage.Collage Msg
renderCountry countryId polygon medianCoordinates color troopCount capitolStatus =
    Collage.group
        [ renderTroopCount medianCoordinates troopCount
        , renderArea polygon color capitolStatus countryId
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


renderArea : List ( Float, Float ) -> Color.Color -> CapitolStatus -> String -> Collage.Collage msg
renderArea polygonPoints color capitolStatus countryId =
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
                |> Collage.outlined (Collage.solid (toFloat defaultScale / 24.0) (Collage.uniform countryBorderColor))

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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LoadingGame _ _ ->
            Time.every 50 (always LoadGame)

        _ ->
            Sub.none



------------- PARSING ----------------
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
                    }
                )
    , bodiesOfWater =
        gameMapWithoutPolygons.bodiesOfWater
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
