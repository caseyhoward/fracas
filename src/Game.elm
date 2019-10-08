module Game exposing
    ( Game
    , CapitolStatus(..)
    , CountryToRender
    , Error
    , Id(..)
    , Player
    , PlayerId(..)
    , PlayerTurn(..)
    , PlayerTurnStage(..)
    , Players
    , canCurrentPlayerPass
    , cancelMovingTroops
    , countryClicked
    , create
    , errorToString
    , findCountryOwner
    , get
    , getAttackStrengthPerPlayer
    , getCountriesToRender
    , getCountryAttackers
    , getCountryDefenders
    , getCountryDefenseStrength
    , getCountryHasPort
    , getCurrentPlayer
    , getDefaultColor
    , getPlayer
    , getPlayerColorFromPlayerTurn
    , getPlayerCountryAndTroopCounts
    , getPlayerTurnStageFromPlayerTurn
    , getTroopCount
    , getTroopCountForCountry
    , idToString
    , isCountryAttacking
    , isCountryDefending
    , isCountryIdCapitol
    , pass
    , playerIdToString
    , playerTurnToString
    ,  playerUrlParser
       -- , start

    , troopsToMove
    , updateNumberOfTroopsToMove
    , urlParser
    )

-- import Color

import Api.Enum.PlayerTurnStage
import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Game
import Api.Object.Player
import Api.Object.PlayerTurn
import Api.Query
import Colors
import Dict
import Map
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet exposing (SelectionSet)
import RemoteData
import Set
import TroopCount
import Url.Parser
import ViewHelpers


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))


idToString : Id -> String
idToString (Id id) =
    id



-- playerIdToString : PlayerId -> String
-- playerIdToString (Id id) =
--     id


playerUrlParser : Url.Parser.Parser (PlayerId -> a) a
playerUrlParser =
    Url.Parser.custom "PLAYERID" (\playerId -> playerId |> String.toInt |> Maybe.map PlayerId)


type alias NewGame =
    { currentPlayerTurn : PlayerTurn
    , map : Map.Map
    , players : Players
    , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
    , numberOfPlayers : Int
    }


type alias Game =
    { id : Id
    , currentPlayerTurn : PlayerTurn
    , map : Map.Map
    , players : Players
    , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
    , numberOfPlayers : Int
    }


type alias CountryToRender =
    { id : Map.CountryId
    , color : Colors.Color
    , troopCount : TroopCount.TroopCount
    , center : Map.ScaledPoint
    , polygonPoints : List Map.ScaledPoint
    , capitolDots : Maybe (Set.Set Map.ScaledPoint)
    , canBeClicked : Bool
    , isBeingMovedFrom : Bool
    , portSegments : Maybe (Set.Set ( Map.ScaledPoint, Map.ScaledPoint ))
    }


type alias Players =
    Dict.Dict Int Player


type Error
    = Error String


type Id
    = Id String


type PlayerId
    = PlayerId Int


type alias Player =
    { name : String
    , countryTroopCounts : Dict.Dict String TroopCount.TroopCount
    , capitolStatus : CapitolStatus
    , color : Colors.Color
    , ports : Set.Set String
    }


type PlayerTurn
    = PlayerTurn PlayerTurnStage PlayerId


create : String -> Int -> Dict.Dict String TroopCount.TroopCount -> (RemoteData.RemoteData (Graphql.Http.Error Id) Id -> msg) -> Cmd msg
create selectedMapId numberOfPlayers neutralTroopCounts toMsg =
    let
        playerTurnInput : Api.InputObject.PlayerTurnInput
        playerTurnInput =
            { playerId = "1"
            , playerTurnStage = Api.Enum.PlayerTurnStage.CapitolPlacement
            }

        players =
            List.range 1 numberOfPlayers
                |> List.map
                    (\playerId ->
                        { id = playerId |> String.fromInt
                        , countryTroopCounts = Dict.empty
                        , name = "Player " ++ String.fromInt playerId
                        , capitolStatus = NoCapitol
                        , color = getDefaultColor (PlayerId playerId)
                        , ports = Set.empty
                        }
                    )

        playersInput : List Api.InputObject.PlayerInput
        playersInput =
            players
                |> List.map
                    (\player ->
                        let
                            fields : Api.InputObject.PlayerInputRequiredFields
                            fields =
                                { id = player.id
                                , countryTroopCounts = player.countryTroopCounts |> TroopCount.troopCountsInput
                                , name = player.name
                                , color = player.color |> Colors.input
                                , ports = []
                                }
                        in
                        Api.InputObject.buildPlayerInput
                            fields
                            identity
                    )

        neutralCountryTroops : List Api.InputObject.CountryTroopCountsInput
        neutralCountryTroops =
            neutralTroopCounts |> TroopCount.troopCountsInput

        input : Api.Mutation.CreateGameRequiredArguments
        input =
            { newGame =
                Api.InputObject.buildGameInput
                    { mapId = selectedMapId
                    , players = playersInput
                    , neutralCountryTroops = neutralCountryTroops
                    , numberOfPlayers = numberOfPlayers
                    , playerTurn = playerTurnInput
                    }
            }

        gameSelectionSet : Graphql.SelectionSet.SelectionSet Id Api.Object.Game
        gameSelectionSet =
            Api.Object.Game.id |> Graphql.SelectionSet.map Id
    in
    Api.Mutation.createGame input gameSelectionSet
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


type alias PlayerSelectionSet =
    { id : PlayerId
    , name : String
    , countryTroopCounts : Dict.Dict String TroopCount.TroopCount
    , capitolStatus : CapitolStatus
    , color : Colors.Color
    , ports : Set.Set String
    }


get : Id -> (RemoteData.RemoteData (Graphql.Http.Error Game) Game -> msg) -> Cmd msg
get (Id id) toMsg =
    let
        playerTurnSelectionSet : SelectionSet PlayerTurn Api.Object.PlayerTurn
        playerTurnSelectionSet =
            Graphql.SelectionSet.map2
                (\playerId playerTurnStage ->
                    PlayerTurn
                        (case playerTurnStage of
                            Api.Enum.PlayerTurnStage.CapitolPlacement ->
                                CapitolPlacement

                            Api.Enum.PlayerTurnStage.TroopPlacement ->
                                TroopPlacement

                            Api.Enum.PlayerTurnStage.AttackAnnexOrPort ->
                                AttackAnnexOrPort

                            Api.Enum.PlayerTurnStage.TroopMovement ->
                                TroopMovement

                            Api.Enum.PlayerTurnStage.GameOver ->
                                GameOver
                        )
                        (playerId |> String.toInt |> Maybe.withDefault 0 |> PlayerId)
                )
                Api.Object.PlayerTurn.playerId
                Api.Object.PlayerTurn.playerTurnStage

        playerSelection : SelectionSet PlayerSelectionSet Api.Object.Player
        playerSelection =
            Graphql.SelectionSet.map6
                (\playerId name countryTroopCounts maybeCapitol color ports ->
                    let
                        capitol =
                            case maybeCapitol of
                                Just countryId ->
                                    Capitol (Map.CountryId countryId)

                                Nothing ->
                                    NoCapitol
                    in
                    { id = playerId |> String.toInt |> Maybe.withDefault 0 |> PlayerId
                    , name = name
                    , countryTroopCounts = countryTroopCounts |> Dict.fromList
                    , capitolStatus = capitol
                    , color = color
                    , ports = ports |> Set.fromList
                    }
                )
                Api.Object.Player.id
                Api.Object.Player.name
                (Api.Object.Player.countryTroopCounts TroopCount.troopCountsSelection)
                Api.Object.Player.capitol
                (Api.Object.Player.color Colors.selectionSet)
                Api.Object.Player.ports

        playerSelectionSetsToPlayers : List PlayerSelectionSet -> Dict.Dict Int Player
        playerSelectionSetsToPlayers playerSelectionSets =
            playerSelectionSets
                |> List.map
                    (\playerSelectionSet ->
                        case playerSelectionSet.id of
                            PlayerId playerId ->
                                ( playerId
                                , { name = playerSelectionSet.name
                                  , countryTroopCounts = playerSelectionSet.countryTroopCounts
                                  , capitolStatus = playerSelectionSet.capitolStatus
                                  , color = playerSelectionSet.color
                                  , ports = playerSelectionSet.ports
                                  }
                                )
                    )
                |> Dict.fromList

        gameSelectionSet : SelectionSet Game Api.Object.Game
        gameSelectionSet =
            Graphql.SelectionSet.map6
                (\id2 map currentPlayerTurn players neutralCountryTroops numberOfPlayers ->
                    let
                        activeGame : Game
                        activeGame =
                            { id = Id id2
                            , currentPlayerTurn = currentPlayerTurn
                            , map = map
                            , players = players |> playerSelectionSetsToPlayers
                            , neutralCountryTroops = neutralCountryTroops |> Dict.fromList
                            , numberOfPlayers = numberOfPlayers
                            }
                    in
                    activeGame
                )
                Api.Object.Game.id
                (Api.Object.Game.map Map.mapSelection)
                (Api.Object.Game.playerTurn playerTurnSelectionSet)
                (Api.Object.Game.players playerSelection)
                (Api.Object.Game.neutralCountryTroops TroopCount.troopCountsSelection)
                Api.Object.Game.numberOfPlayers

        query : SelectionSet Game Graphql.Operation.RootQuery
        query =
            Api.Query.game { id = id } gameSelectionSet
    in
    query
        |> Graphql.Http.queryRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


errorToString : Error -> String
errorToString (Error error) =
    error


countryClicked : Map.CountryId -> Game -> Result Error Game
countryClicked clickedCountryId activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn playerTurnStage currentPlayerId ->
            case playerTurnStage of
                CapitolPlacement ->
                    attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame

                TroopPlacement ->
                    attemptTroopPlacement clickedCountryId currentPlayerId (numberOfTroopsToPlace currentPlayerId activeGame.players) activeGame

                AttackAnnexOrPort ->
                    attackAnnexOrPort clickedCountryId currentPlayerId activeGame

                TroopMovement ->
                    attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame

                TroopMovementFromSelected fromCountryId numberOfTroopsToMoveString ->
                    attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame

                GameOver ->
                    Ok activeGame


getAttackStrengthPerPlayer : Map.Map -> Players -> Map.CountryId -> Dict.Dict Int TroopCount.TroopCount
getAttackStrengthPerPlayer gameMap players countryId =
    getCountryAttackers gameMap players countryId
        |> Dict.map
            (\_ attacker ->
                let
                    neighborAttack =
                        attacker.neighboringCountryAttackers
                            |> Dict.foldl
                                (\_ troopCount result ->
                                    TroopCount.addTroopCounts troopCount result
                                )
                                TroopCount.noTroops

                    waterAttack =
                        attacker.neighboringThroughWaterAttackers
                            |> Dict.foldl
                                (\_ troopCount result ->
                                    TroopCount.addTroopCounts (troopCount |> TroopCount.acrossWater) result
                                )
                                TroopCount.noTroops
                in
                TroopCount.addTroopCounts neighborAttack waterAttack
            )


getCountryAttackers : Map.Map -> Players -> Map.CountryId -> CountryAttackers
getCountryAttackers gameMap players countryId =
    let
        neighborCountriesByPlayer : List ( PlayerId, Map.CountryId )
        neighborCountriesByPlayer =
            case Map.getCountry countryId gameMap.countries of
                Just country ->
                    country.neighboringCountries
                        |> Set.foldl
                            (\neighborCountry result ->
                                case findCountryOwner (Map.CountryId neighborCountry) players of
                                    Just neighborId ->
                                        ( neighborId, Map.CountryId neighborCountry ) :: result

                                    Nothing ->
                                        result
                            )
                            []

                Nothing ->
                    []

        neighborCountryTroopCountsByPlayer : List ( PlayerId, Map.CountryId, TroopCount.TroopCount )
        neighborCountryTroopCountsByPlayer =
            neighborCountriesByPlayer
                |> List.map
                    (\( playerId, neighborCountryId ) ->
                        ( playerId
                        , neighborCountryId
                        , getTroopCountForCountry neighborCountryId players |> Maybe.withDefault TroopCount.noTroops
                        )
                    )

        neighboringCountryAttackers : Dict.Dict Int (Dict.Dict String TroopCount.TroopCount)
        neighboringCountryAttackers =
            neighborCountryTroopCountsByPlayer
                |> List.foldl
                    (\( PlayerId playerId, Map.CountryId neighborCountryId, troopCount ) result ->
                        case Dict.get playerId result of
                            Just troopCounts ->
                                result |> Dict.insert playerId (Dict.insert neighborCountryId troopCount troopCounts)

                            Nothing ->
                                result
                                    |> Dict.insert playerId
                                        (Dict.fromList
                                            [ ( neighborCountryId, troopCount ) ]
                                        )
                    )
                    Dict.empty

        countriesReachableThroughWater : List Map.CountryId
        countriesReachableThroughWater =
            Map.getCountriesThatCanReachCountryThroughWater gameMap countryId

        attackerCountriesNeighoboringWater : List Map.CountryId
        attackerCountriesNeighoboringWater =
            countriesReachableThroughWater

        attackerCountriesNeighoboringWaterWithPort : List Map.CountryId
        attackerCountriesNeighoboringWaterWithPort =
            attackerCountriesNeighoboringWater
                |> filterCountriesWithPort players

        waterNeighborCountriesByPlayer : List ( PlayerId, Map.CountryId )
        waterNeighborCountriesByPlayer =
            attackerCountriesNeighoboringWaterWithPort
                |> List.foldl
                    (\waterNeighborCountry result ->
                        case findCountryOwner waterNeighborCountry players of
                            Just neighborId ->
                                ( neighborId, waterNeighborCountry ) :: result

                            Nothing ->
                                result
                    )
                    []

        waterNeighborCountriesByPlayerTroopCounts : List ( PlayerId, Map.CountryId, TroopCount.TroopCount )
        waterNeighborCountriesByPlayerTroopCounts =
            waterNeighborCountriesByPlayer
                |> List.map
                    (\( playerId, neighborCountryId ) ->
                        ( playerId
                        , neighborCountryId
                        , getTroopCountForCountry neighborCountryId players
                            |> Maybe.withDefault TroopCount.noTroops
                          -- TODO
                        )
                    )

        waterNeighborAttackers : Dict.Dict Int (Dict.Dict String TroopCount.TroopCount)
        waterNeighborAttackers =
            waterNeighborCountriesByPlayerTroopCounts
                |> List.foldl
                    (\( PlayerId playerId, Map.CountryId neighborCountryId, troopCount ) result ->
                        case Dict.get playerId result of
                            Just troopCounts ->
                                result |> Dict.insert playerId (Dict.insert neighborCountryId troopCount troopCounts)

                            Nothing ->
                                result
                                    |> Dict.insert playerId
                                        (Dict.fromList
                                            [ ( neighborCountryId, troopCount ) ]
                                        )
                    )
                    Dict.empty
    in
    players
        |> Dict.foldl
            (\playerId _ result ->
                result
                    |> Dict.insert
                        playerId
                        { neighboringCountryAttackers = Dict.get playerId neighboringCountryAttackers |> Maybe.withDefault Dict.empty
                        , neighboringThroughWaterAttackers = Dict.get playerId waterNeighborAttackers |> Maybe.withDefault Dict.empty
                        }
            )
            Dict.empty


getCountriesToRender : Map.Map -> Players -> PlayerTurn -> Dict.Dict String TroopCount.TroopCount -> Maybe (List CountryToRender)
getCountriesToRender gameMap players currentPlayerTurn neutralCountryTroops =
    gameMap.countries
        |> Map.scaledCountries ViewHelpers.pixelsPerMapSquare
        |> Dict.map
            (\countryId country ->
                let
                    countryOwnerAndTroopCount =
                        findCountryOwnerAndTroopCount (Map.CountryId countryId) players

                    -- shift =
                    --     \_ ->
                    --         case country.center of
                    --             ( medianX, medianY ) ->
                    --                 ( medianX * ViewHelpers.pixelsPerMapSquare, medianY * ViewHelpers.pixelsPerMapSquare )
                    -- ( (toFloat medianX + 0.5) * toFloat ViewHelpers.pixelsPerMapSquare, (toFloat medianY + 0.5) * toFloat ViewHelpers.pixelsPerMapSquare )
                in
                case countryOwnerAndTroopCount of
                    Just ( countryOwnerId, troopCount ) ->
                        getPlayer countryOwnerId players
                            |> Maybe.map
                                (\countryOwner ->

                                    { id = Map.CountryId countryId
                                    , troopCount = troopCount
                                    , center = country.center
                                    , polygonPoints = country.polygon
                                    , color = countryOwner.color
                                    , capitolDots =
                                        case countryOwner.capitolStatus of
                                            Capitol (Map.CountryId capitolId) ->
                                                if capitolId == countryId then
                                                    Just country.coordinates

                                                else
                                                    Nothing

                                            NoCapitol ->
                                                Nothing
                                    , canBeClicked = getCountryCanBeClicked currentPlayerTurn players gameMap (Map.CountryId countryId)
                                    , isBeingMovedFrom = getIsBeingMovedFrom currentPlayerTurn (Map.CountryId countryId)
                                    , portSegments =
                                        getCountryHasPort (Map.CountryId countryId) players
                                            |> Maybe.andThen
                                                (\hasPort ->
                                                    if hasPort then
                                                        Just country.waterEdges

                                                    else
                                                        Nothing
                                                )
                                    }
                                )

                    Nothing ->
                        Just
                            { id = Map.CountryId countryId
                            , troopCount = getTroopCount (Map.CountryId countryId) neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops
                            , center = country.center
                            , color = neutralCountryColor
                            , polygonPoints = country.polygon
                            , capitolDots = Nothing
                            , canBeClicked = getCountryCanBeClicked currentPlayerTurn players gameMap (Map.CountryId countryId)
                            , isBeingMovedFrom = False
                            , portSegments = Nothing
                            }
            )
        |> Dict.values
        |> List.foldl
            (\maybeCountryToRender result ->
                case ( result, maybeCountryToRender ) of
                    ( Just countriesToRender, Just countryToRender ) ->
                        Just (countryToRender :: countriesToRender)

                    _ ->
                        Nothing
            )
            (Just [])



-- Exposed


cancelMovingTroops : Game -> Game
cancelMovingTroops activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn _ playerId ->
            { activeGame | currentPlayerTurn = PlayerTurn TroopMovement playerId }


canCurrentPlayerPass : PlayerTurn -> Bool
canCurrentPlayerPass currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn playerTurnStage _ ->
            case playerTurnStage of
                TroopMovement ->
                    True

                AttackAnnexOrPort ->
                    True

                _ ->
                    False


getCurrentPlayer : PlayerTurn -> PlayerId
getCurrentPlayer currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn _ playerId ->
            playerId


isCountryDefending : Map.Map -> Players -> Map.CountryId -> Map.CountryId -> Bool
isCountryDefending gameMap players countryToDefend (Map.CountryId countryThatMightDefend) =
    let
        countryDefense =
            getCountryDefenders players gameMap countryToDefend

        defendingCountries =
            Dict.keys countryDefense.neighboringCountryDefense ++ Dict.keys countryDefense.neighboringThroughWaterDefense
    in
    defendingCountries |> Set.fromList |> Set.member countryThatMightDefend


isCountryAttacking : Map.Map -> Players -> Map.CountryId -> Map.CountryId -> Bool
isCountryAttacking gameMap players countryToDefend (Map.CountryId countryThatMightDefend) =
    let
        countryAttackers =
            getCountryAttackers gameMap players countryToDefend

        attackingCountries : List String
        attackingCountries =
            countryAttackers
                |> Dict.foldl
                    (\_ attacker result ->
                        result ++ Dict.keys attacker.neighboringCountryAttackers ++ Dict.keys attacker.neighboringThroughWaterAttackers
                    )
                    []
    in
    attackingCountries |> Set.fromList |> Set.member countryThatMightDefend


getCountryDefenders : Players -> Map.Map -> Map.CountryId -> CountryDefenders
getCountryDefenders players gameMap countryId =
    let
        playerId : PlayerId
        playerId =
            case findCountryOwner countryId players of
                Just ownerId ->
                    ownerId

                Nothing ->
                    PlayerId -1

        defendingCountryTroopCount : TroopCount.TroopCount
        defendingCountryTroopCount =
            case getTroopCountForCountry countryId players of
                Just countryBeingAttackedTroopCount ->
                    countryBeingAttackedTroopCount

                Nothing ->
                    -- This shouldn't happen
                    TroopCount.noTroops

        neigboringCountryDefense : Dict.Dict String TroopCount.TroopCount
        neigboringCountryDefense =
            case Map.getCountry countryId gameMap.countries of
                Just countryBeingAttacked ->
                    countryBeingAttacked.neighboringCountries
                        |> Set.toList
                        |> List.map (\id -> Map.CountryId id)
                        |> List.foldl
                            (\(Map.CountryId neighboringCountryId) defense ->
                                if isCountryOwnedByPlayer playerId (Map.CountryId neighboringCountryId) players then
                                    case getTroopCountForCountry (Map.CountryId neighboringCountryId) players of
                                        Just neighboringCountryTroopCount ->
                                            Dict.insert neighboringCountryId neighboringCountryTroopCount defense

                                        Nothing ->
                                            defense

                                else
                                    defense
                            )
                            Dict.empty

                Nothing ->
                    -- This shouldn't happen
                    Dict.empty

        defenseThroughWater =
            getDefenseThroughWater gameMap players countryId
                |> Dict.filter
                    (\throughWaterCountryId _ ->
                        case Map.getCountry countryId gameMap.countries of
                            Just countryBeingAttacked ->
                                not <| Set.member throughWaterCountryId countryBeingAttacked.neighboringCountries

                            Nothing ->
                                True
                     -- TODO
                    )
    in
    { neighboringCountryDefense = neigboringCountryDefense
    , neighboringThroughWaterDefense = defenseThroughWater
    , countryDefense = defendingCountryTroopCount
    }


getCountryDefenseStrength : Map.Map -> Players -> Map.CountryId -> TroopCount.TroopCount
getCountryDefenseStrength gameMap players countryId =
    let
        countryDefense =
            getCountryDefenders players gameMap countryId

        neighborDefense =
            countryDefense.neighboringThroughWaterDefense
                |> Dict.values
                |> List.foldl (\troopCount result -> TroopCount.addTroopCounts (troopCount |> TroopCount.acrossWater) result) TroopCount.noTroops

        waterDefense =
            countryDefense.neighboringCountryDefense
                |> Dict.values
                |> List.foldl (\troopCount result -> TroopCount.addTroopCounts troopCount result) TroopCount.noTroops
    in
    countryDefense.countryDefense
        |> TroopCount.addTroopCounts neighborDefense
        |> TroopCount.addTroopCounts waterDefense


findCountryOwner : Map.CountryId -> Players -> Maybe PlayerId
findCountryOwner countryId players =
    findCountryOwnerAndTroopCount countryId players
        |> Maybe.map Tuple.first


getCountryHasPort : Map.CountryId -> Players -> Maybe Bool
getCountryHasPort (Map.CountryId countryId) players =
    findCountryOwner (Map.CountryId countryId) players
        |> Maybe.andThen
            (\playerId ->
                getPlayer playerId players
                    |> Maybe.map .ports
                    |> Maybe.map
                        (Set.member countryId)
            )


getDefaultColor : PlayerId -> Colors.Color
getDefaultColor (PlayerId playerId) =
    case Dict.get playerId defaultPlayerColors of
        Just color ->
            color

        Nothing ->
            Colors.black


getPlayerCountryAndTroopCounts :
    { players : Players, currentPlayerTurn : PlayerTurn }
    -> List { playerId : PlayerId, countryCount : Int, troopCount : TroopCount.TroopCount, isAlive : Bool }
getPlayerCountryAndTroopCounts { players, currentPlayerTurn } =
    players
        |> Dict.map
            (\playerId player ->
                case player.capitolStatus of
                    Capitol _ ->
                        { playerId = PlayerId playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = True
                        }

                    NoCapitol ->
                        { playerId = PlayerId playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = False || isCapitolPlacementTurn currentPlayerTurn
                        }
            )
        |> Dict.values


getTotalTroopCountForPlayer : Player -> TroopCount.TroopCount
getTotalTroopCountForPlayer player =
    player.countryTroopCounts
        |> Dict.values
        |> List.foldl (\troopCount result -> TroopCount.addTroopCounts troopCount result) TroopCount.noTroops


getPlayer : PlayerId -> Players -> Maybe Player
getPlayer (PlayerId playerId) players =
    Dict.get playerId players


getTroopCount : Map.CountryId -> Dict.Dict String TroopCount.TroopCount -> Maybe TroopCount.TroopCount
getTroopCount (Map.CountryId countryId) troopCounts =
    Dict.get countryId troopCounts


getTroopCountForCountry : Map.CountryId -> Players -> Maybe TroopCount.TroopCount
getTroopCountForCountry countryId players =
    findCountryOwner countryId players
        |> Maybe.andThen (\playerId -> getPlayer playerId players)
        |> Maybe.andThen (\player -> getTroopCount countryId player.countryTroopCounts)


getPlayerColorFromPlayerTurn : Players -> PlayerTurn -> Colors.Color
getPlayerColorFromPlayerTurn players playerTurn =
    case playerTurn of
        PlayerTurn _ playerId ->
            getPlayer playerId players
                |> Maybe.map
                    (\player ->
                        player.color
                    )
                |> Maybe.withDefault Colors.black


getPlayerTurnStageFromPlayerTurn : PlayerTurn -> PlayerTurnStage
getPlayerTurnStageFromPlayerTurn playerTurn =
    case playerTurn of
        PlayerTurn playerTurnStage _ ->
            playerTurnStage


isCapitolPlacementTurn : PlayerTurn -> Bool
isCapitolPlacementTurn currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn CapitolPlacement _ ->
            True

        _ ->
            False


isCountryIdCapitol : PlayerId -> Map.CountryId -> Players -> Maybe Bool
isCountryIdCapitol playerId countryId players =
    getPlayer playerId players
        |> Maybe.map
            (\player ->
                case player.capitolStatus of
                    Capitol capitolId ->
                        capitolId == countryId

                    NoCapitol ->
                        False
            )


numberOfTroopsToPlace : PlayerId -> Players -> TroopCount.TroopCount
numberOfTroopsToPlace playerId players =
    case getPlayer playerId players of
        Just player ->
            TroopCount.numberOfTroopsToPlace (Dict.size player.countryTroopCounts) troopsPerCountryPerTurn

        Nothing ->
            -- TODO : Propogate error
            TroopCount.noTroops


pass : Game -> Result Error Game
pass activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected _ _) playerId ->
            Ok { activeGame | currentPlayerTurn = PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }

        PlayerTurn TroopMovement playerId ->
            Ok { activeGame | currentPlayerTurn = PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }

        PlayerTurn AttackAnnexOrPort playerId ->
            Ok
                { activeGame
                    | currentPlayerTurn =
                        if playerHasMoreThanOneCountry activeGame.players playerId then
                            PlayerTurn TroopMovement playerId

                        else
                            PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                }

        _ ->
            "Can't pass" |> Error |> Err


playerIdToString : PlayerId -> String
playerIdToString (PlayerId playerId) =
    String.fromInt playerId


playerTurnToString : Players -> PlayerTurn -> String
playerTurnToString players (PlayerTurn playerTurnStage playerId) =
    case getPlayerName playerId players of
        Just playerName ->
            case playerTurnStage of
                CapitolPlacement ->
                    playerName ++ ": Choose your first country. This country will be your capitol. If it is captured, you lose."

                TroopPlacement ->
                    playerName ++ ": Place " ++ (numberOfTroopsToPlace playerId players |> TroopCount.pluralize) ++ " in one of your countries"

                AttackAnnexOrPort ->
                    playerName ++ ": Choose an enemy country to attack, a neutral country to annex, or one of your countries bordering water to build a port"

                TroopMovement ->
                    playerName ++ ": Choose a country to move troops from or press the \"Pass\" button for no troop movement"

                TroopMovementFromSelected _ _ ->
                    playerName ++ ": Enter the number of troops to move and choose a destination or press the \"Pass\" button for no movement"

                GameOver ->
                    playerName ++ " has won the game!!!"

        Nothing ->
            -- TODO
            ""



-- start : Dict.Dict String Map.Map -> Map.Id -> Int -> Dict.Dict String TroopCount.TroopCount -> Result Error Game
-- start gameMaps gameMapId numberOfPlayers neutralTroopCounts =
--     case Map.get gameMapId gameMaps of
--         Ok gameMap ->
--             Ok
--                 { map = gameMap
--                 , players =
--                     List.range 1 numberOfPlayers
--                         |> List.map
--                             (\playerId ->
--                                 ( playerId
--                                 , { countryTroopCounts = Dict.empty
--                                   , name = "Player " ++ String.fromInt playerId
--                                   , capitolStatus = NoCapitol
--                                   , color = getDefaultColor (PlayerId playerId)
--                                   , ports = Set.empty
--                                   }
--                                 )
--                             )
--                         |> Dict.fromList
--                 , currentPlayerTurn = PlayerTurn CapitolPlacement (PlayerId 1)
--                 , numberOfPlayers = numberOfPlayers
--                 , neutralCountryTroops = neutralTroopCounts
--                 }
--         Err error ->
--             error |> Map.errorToString |> Error |> Err


troopsToMove : PlayerTurn -> Maybe String
troopsToMove currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected _ troops) _ ->
            Just troops

        _ ->
            Nothing


updateNumberOfTroopsToMove : String -> Game -> Game
updateNumberOfTroopsToMove numberOfTroopsToMoveString activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected countryId _) currentPlayerId ->
            { activeGame
                | currentPlayerTurn =
                    currentPlayerId |> PlayerTurn (TroopMovementFromSelected countryId numberOfTroopsToMoveString)
            }

        _ ->
            activeGame



------- LOCAL


type PlayerTurnStage
    = CapitolPlacement
    | TroopPlacement
    | AttackAnnexOrPort
    | TroopMovement
    | TroopMovementFromSelected Map.CountryId String
    | GameOver


type CapitolStatus
    = NoCapitol
    | Capitol Map.CountryId


type CountryStatus
    = Unoccupied
    | OccupiedByOpponent PlayerId
    | OccupiedByCurrentPlayer TroopCount.TroopCount


type AttackResult
    = CurrentPlayerAcquiresOpponentCountry
    | OpponentCountryLosesTroops TroopCount.TroopCount
    | OpponentEliminated
    | NotEnoughTroopsToAttack TroopCount.TroopCount TroopCount.TroopCount
    | AttackResultError String


type alias CountryDefenders =
    { neighboringCountryDefense : Dict.Dict String TroopCount.TroopCount
    , neighboringThroughWaterDefense : Dict.Dict String TroopCount.TroopCount
    , countryDefense : TroopCount.TroopCount
    }


type alias CountryAttackers =
    Dict.Dict Int CountryAttackersForPlayer


type alias CountryAttackersForPlayer =
    { neighboringCountryAttackers : Dict.Dict String TroopCount.TroopCount
    , neighboringThroughWaterAttackers : Dict.Dict String TroopCount.TroopCount
    }


addPortForPlayer : Map.CountryId -> Player -> Player
addPortForPlayer (Map.CountryId countryId) player =
    { player | ports = player.ports |> Set.insert countryId }


attackAnnexOrPort : Map.CountryId -> PlayerId -> Game -> Result Error Game
attackAnnexOrPort clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer _ ->
            attemptToBuildPort currentPlayerId clickedCountryId activeGame

        OccupiedByOpponent opponentPlayerId ->
            attemptToAttackCountry opponentPlayerId clickedCountryId activeGame

        Unoccupied ->
            attemptToAnnexCountry currentPlayerId clickedCountryId activeGame


attemptTroopMovement : Map.CountryId -> Map.CountryId -> String -> Game -> Result Error Game
attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer playerCountryToTroopCount ->
            case String.toInt numberOfTroopsToMoveString of
                Just numberOfTroopsToMove ->
                    if isCountryReachableFromOtherCountry fromCountryId clickedCountryId (getCurrentPlayer activeGame.currentPlayerTurn) activeGame.map.countries activeGame.players then
                        let
                            fromCountryTroopCount =
                                case getPlayer (getCurrentPlayer activeGame.currentPlayerTurn) activeGame.players of
                                    Just currentPlayer1 ->
                                        case getTroopCount fromCountryId currentPlayer1.countryTroopCounts of
                                            Just troopCount ->
                                                troopCount

                                            Nothing ->
                                                TroopCount.noTroops

                                    Nothing ->
                                        TroopCount.noTroops

                            allowedNumberOfTroopsToMove =
                                TroopCount.numberOfTroopsToMove fromCountryTroopCount numberOfTroopsToMove

                            updatedGameResult =
                                activeGame
                                    |> updatePlayerTroopCountForCountry (getCurrentPlayer activeGame.currentPlayerTurn) fromCountryId (TroopCount.subtractTroopCounts allowedNumberOfTroopsToMove fromCountryTroopCount)
                                    |> Result.andThen (updatePlayerTroopCountForCountry (getCurrentPlayer activeGame.currentPlayerTurn) clickedCountryId (TroopCount.addTroopCounts playerCountryToTroopCount allowedNumberOfTroopsToMove))
                        in
                        updatedGameResult
                            |> Result.map
                                (\updatedGame ->
                                    { updatedGame
                                        | currentPlayerTurn = PlayerTurn TroopPlacement (getCurrentPlayer activeGame.currentPlayerTurn |> nextPlayerCheckForDeadPlayers activeGame.players)
                                    }
                                )

                    else
                        "You can't move troops between those countries" |> Error |> Err

                Nothing ->
                    "Number of troops must be a number" |> Error |> Err

        _ ->
            "You must move troops to your own country" |> Error |> Err


attemptToPlaceCapitol : Map.CountryId -> PlayerId -> Game -> Result Error Game
attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame =
    case Map.getCountry clickedCountryId activeGame.map.countries of
        Just clickedCountry ->
            case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
                OccupiedByCurrentPlayer _ ->
                    "Error: Somehow you are placing a second capitol" |> Error |> Err

                OccupiedByOpponent _ ->
                    "You must select an unoccuppied country" |> Error |> Err

                Unoccupied ->
                    case getPlayer currentPlayerId activeGame.players of
                        Just currentPlayer ->
                            let
                                neutralTroopCount =
                                    getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

                                updatedPlayer =
                                    { currentPlayer
                                        | countryTroopCounts =
                                            updateTroopCount clickedCountryId neutralTroopCount currentPlayer.countryTroopCounts
                                        , capitolStatus = Capitol clickedCountryId

                                        -- , capitolStatus = Capitol clickedCountryId (Map.capitolDotsCoordinates clickedCountry.coordinates ViewHelpers.pixelsPerMapSquare)
                                    }

                                updatedPlayers =
                                    updatePlayer currentPlayerId updatedPlayer activeGame.players

                                nextPlayerId =
                                    case currentPlayerId of
                                        PlayerId id ->
                                            PlayerId (remainderBy (Dict.size updatedPlayers) id + 1)

                                nextPlayerTurn =
                                    case currentPlayerId of
                                        PlayerId id ->
                                            if id == Dict.size updatedPlayers then
                                                PlayerTurn TroopPlacement nextPlayerId

                                            else
                                                PlayerTurn CapitolPlacement nextPlayerId
                            in
                            Ok
                                { activeGame
                                    | players = updatedPlayers
                                    , neutralCountryTroops = destroyTroops clickedCountryId activeGame.neutralCountryTroops
                                    , currentPlayerTurn = nextPlayerTurn
                                }

                        Nothing ->
                            "Something bad happened" |> Error |> Err

        _ ->
            "Something bad happened" |> Error |> Err


attemptTroopPlacement : Map.CountryId -> PlayerId -> TroopCount.TroopCount -> Game -> Result Error Game
attemptTroopPlacement clickedCountryId currentPlayerId troopsToPlace activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer clickedCountryTroopCount ->
            let
                updatedGameResult =
                    activeGame |> updatePlayerTroopCountForCountry currentPlayerId clickedCountryId (TroopCount.addTroopCounts clickedCountryTroopCount troopsToPlace)
            in
            updatedGameResult
                |> Result.map
                    (\updatedGame ->
                        { updatedGame | currentPlayerTurn = PlayerTurn AttackAnnexOrPort currentPlayerId }
                    )

        OccupiedByOpponent _ ->
            "You must put troops in your own country" |> Error |> Err

        Unoccupied ->
            "You must put troops in your own country" |> Error |> Err



-- TODO Make this return BuildPortResult instead


attemptToBuildPort : PlayerId -> Map.CountryId -> Game -> Result Error Game
attemptToBuildPort currentPlayerId clickedCountryId activeGame =
    case getTroopCountForCountry clickedCountryId activeGame.players of
        Just _ ->
            buildPort currentPlayerId clickedCountryId activeGame

        Nothing ->
            "You can't build a port in a country you don't own" |> Error |> Err


attackResult : Map.CountryId -> Map.Map -> Players -> PlayerTurn -> AttackResult
attackResult clickedCountryId gameMap players currentPlayerTurn =
    case findCountryOwner clickedCountryId players of
        Just opponentPlayerId ->
            let
                countryAttackers =
                    getAttackStrengthPerPlayer gameMap players clickedCountryId

                currentPlayerId =
                    getCurrentPlayer currentPlayerTurn

                currentPlayerIdInt =
                    case currentPlayerId of
                        PlayerId playerId ->
                            playerId

                attackStrength =
                    case Dict.get currentPlayerIdInt countryAttackers of
                        Just attack ->
                            attack

                        Nothing ->
                            TroopCount.noTroops

                defenseStrength =
                    getCountryDefenseStrength gameMap players clickedCountryId

                remainingTroops =
                    getTroopCountForCountry clickedCountryId players
                        |> Maybe.withDefault TroopCount.noTroops
                        |> TroopCount.addTroopCounts defenseStrength
                        |> TroopCount.subtractTroopCounts attackStrength
            in
            if TroopCount.canAttack attackStrength defenseStrength then
                if TroopCount.hasTroops remainingTroops then
                    OpponentCountryLosesTroops remainingTroops

                else
                    case isCountryIdCapitol opponentPlayerId clickedCountryId players of
                        Just isCapitol ->
                            if isCapitol then
                                OpponentEliminated

                            else
                                CurrentPlayerAcquiresOpponentCountry

                        Nothing ->
                            AttackResultError "Error checking if capitol"

            else
                NotEnoughTroopsToAttack attackStrength defenseStrength

        Nothing ->
            AttackResultError "Error finding owner"


attemptSelectTroopMovementFromCountry : Map.CountryId -> PlayerId -> Game -> Result Error Game
attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer troopCount ->
            if TroopCount.hasTroops troopCount then
                Ok
                    { activeGame
                        | currentPlayerTurn =
                            PlayerTurn (TroopMovementFromSelected clickedCountryId (TroopCount.toString troopCount)) currentPlayerId
                    }

            else
                "Select a country with troops" |> Error |> Err

        _ ->
            "You must move troops from your own country" |> Error |> Err


attemptToAnnexCountry : PlayerId -> Map.CountryId -> Game -> Result Error Game
attemptToAnnexCountry currentPlayerId clickedCountryId activeGame =
    if canAnnexCountry activeGame.map currentPlayerId activeGame.players clickedCountryId then
        let
            neutralTroopCount =
                getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

            updatedGameResult =
                updatePlayerTroopCountForCountry currentPlayerId clickedCountryId neutralTroopCount activeGame
        in
        updatedGameResult
            |> Result.map
                (\updatedGame ->
                    { updatedGame
                        | currentPlayerTurn = PlayerTurn TroopMovement currentPlayerId
                        , neutralCountryTroops = removeTroopCount clickedCountryId activeGame.neutralCountryTroops
                    }
                )

    else
        "You can't annex that country" |> Error |> Err


attemptToAttackCountry : PlayerId -> Map.CountryId -> Game -> Result Error Game
attemptToAttackCountry opponentPlayerId clickedCountryId activeGame =
    case attackResult clickedCountryId activeGame.map activeGame.players activeGame.currentPlayerTurn of
        OpponentCountryLosesTroops remainingTroops ->
            activeGame
                |> updatePlayerTroopCountForCountry opponentPlayerId clickedCountryId remainingTroops
                |> Result.map updateForSuccessfulAttack

        OpponentEliminated ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> Result.andThen (destroyPlayer opponentPlayerId)
                |> Result.map updateForSuccessfulAttack

        CurrentPlayerAcquiresOpponentCountry ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> Result.map updateForSuccessfulAttack

        NotEnoughTroopsToAttack attackStrength defenseStrength ->
            ("Not enough to attack: attack strength = " ++ TroopCount.toString attackStrength ++ ", defense strength = " ++ TroopCount.toString defenseStrength)
                |> Error
                |> Err

        AttackResultError errorMessage ->
            errorMessage |> Error |> Err


defaultPlayerColors : Dict.Dict Int Colors.Color
defaultPlayerColors =
    Dict.fromList
        [ ( 1, Colors.darkGreen )
        , ( 3, Colors.lightGreen )
        , ( 2, Colors.lightYellow )
        , ( 5, Colors.orange )
        , ( 4, Colors.brown )
        , ( 6, Colors.lightPurple )
        ]


filterCountriesOwnedBy : Players -> PlayerId -> List Map.CountryId -> List Map.CountryId
filterCountriesOwnedBy players playerId countryIds =
    let
        countriesOwnedByPlayer : Set.Set String
        countriesOwnedByPlayer =
            case getPlayer playerId players of
                Just player ->
                    player.countryTroopCounts |> Dict.keys |> Set.fromList

                Nothing ->
                    Set.empty
    in
    List.foldl
        (\(Map.CountryId countryId) result ->
            if Set.member countryId countriesOwnedByPlayer then
                Map.CountryId countryId :: result

            else
                result
        )
        []
        countryIds


buildPort : PlayerId -> Map.CountryId -> Game -> Result Error Game
buildPort playerId countryId activeGame =
    -- We already had to check that the player owned this country before so no need to do that here
    case Map.isCountryNeighboringWater countryId activeGame.map.countries of
        Just isNeighboringWater ->
            if isNeighboringWater then
                case getCountryHasPort countryId activeGame.players of
                    Just hasPort ->
                        if hasPort then
                            "This country already has a port" |> Error |> Err

                        else
                            let
                                updatedGameResult =
                                    activeGame
                                        |> updatePlayersWithPlayer playerId (addPortForPlayer countryId)

                                nextPlayerTurn =
                                    if playerHasMoreThanOneCountry activeGame.players playerId then
                                        PlayerTurn TroopMovement playerId

                                    else
                                        PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                            in
                            updatedGameResult
                                |> Result.map (\updated -> { updated | currentPlayerTurn = nextPlayerTurn })

                    Nothing ->
                        "Error while building port" |> Error |> Err

            else
                "A country must be next to water to build a port" |> Error |> Err

        Nothing ->
            "Error checking if country borders water" |> Error |> Err


canAnnexCountry : Map.Map -> PlayerId -> Players -> Map.CountryId -> Bool
canAnnexCountry gameMap playerId players countryIdToAnnex =
    -- We already know the country is unoccuppied from an earlier check so just make sure it is reachable from one of the current players countries
    case getPlayer playerId players of
        Just player ->
            player.countryTroopCounts
                |> Dict.foldl
                    (\playerCountryId _ isReachable ->
                        isReachable || isCountryReachableFromOtherCountry (Map.CountryId playerCountryId) countryIdToAnnex playerId gameMap.countries players
                    )
                    False

        Nothing ->
            -- This should never happen
            False


destroyPlayer : PlayerId -> Game -> Result Error Game
destroyPlayer playerId activeGame =
    -- Make this return result with error if dict lookup fails
    activeGame
        |> updatePlayersWithPlayer
            playerId
            (\player ->
                { player | capitolStatus = NoCapitol, countryTroopCounts = Dict.empty }
            )


destroyTroops : Map.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
destroyTroops (Map.CountryId countryId) neutralTroopCounts =
    Dict.remove countryId neutralTroopCounts


filterCountriesWithPort : Players -> List Map.CountryId -> List Map.CountryId
filterCountriesWithPort players countries =
    countries
        |> List.filter
            (\countryId ->
                case getCountryHasPort countryId players of
                    Just hasPort ->
                        hasPort

                    _ ->
                        False
            )


findCountryOwnerAndTroopCount : Map.CountryId -> Players -> Maybe ( PlayerId, TroopCount.TroopCount )
findCountryOwnerAndTroopCount (Map.CountryId countryId) players =
    players
        |> Dict.foldl
            (\playerId player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countryTroopCounts
                            |> Maybe.map (\troopCount -> ( PlayerId playerId, troopCount ))
            )
            Nothing


getCountryCanBeClicked : PlayerTurn -> Players -> Map.Map -> Map.CountryId -> Bool
getCountryCanBeClicked currentPlayerTurn players gameMap countryId =
    case currentPlayerTurn of
        PlayerTurn playerTurnStatus currentPlayerId ->
            case playerTurnStatus of
                CapitolPlacement ->
                    case findCountryOwner countryId players of
                        Just _ ->
                            False

                        Nothing ->
                            True

                TroopPlacement ->
                    case findCountryOwner countryId players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                AttackAnnexOrPort ->
                    let
                        canAttack =
                            case attackResult countryId gameMap players currentPlayerTurn of
                                NotEnoughTroopsToAttack _ _ ->
                                    False

                                AttackResultError _ ->
                                    False

                                _ ->
                                    True

                        canAnnex =
                            canAnnexCountry gameMap currentPlayerId players countryId

                        canBuildPort =
                            False
                    in
                    canAttack || canAnnex || canBuildPort

                TroopMovement ->
                    case findCountryOwner countryId players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                TroopMovementFromSelected fromCountryId _ ->
                    if isCountryReachableFromOtherCountry fromCountryId countryId (getCurrentPlayer currentPlayerTurn) gameMap.countries players then
                        case getPlayer (getCurrentPlayer currentPlayerTurn) players of
                            Just currentPlayer ->
                                case getTroopCount countryId currentPlayer.countryTroopCounts of
                                    Just _ ->
                                        True

                                    Nothing ->
                                        False

                            Nothing ->
                                False

                    else
                        False

                _ ->
                    False


getCountryStatus : Map.CountryId -> Players -> PlayerTurn -> CountryStatus
getCountryStatus countryId players currentPlayerTurn =
    case getPlayer (getCurrentPlayer currentPlayerTurn) players of
        Just currentPlayer ->
            case getTroopCount countryId currentPlayer.countryTroopCounts of
                Just troopCount ->
                    OccupiedByCurrentPlayer troopCount

                Nothing ->
                    case
                        players
                            |> Dict.foldl
                                (\playerId player result ->
                                    case result of
                                        Just _ ->
                                            result

                                        Nothing ->
                                            getTroopCount countryId player.countryTroopCounts
                                                |> Maybe.map (\_ -> OccupiedByOpponent (PlayerId playerId))
                                )
                                Nothing
                    of
                        Just occupiedByOppenent ->
                            occupiedByOppenent

                        Nothing ->
                            Unoccupied

        Nothing ->
            Unoccupied


getDefenseThroughWater : Map.Map -> Players -> Map.CountryId -> Dict.Dict String TroopCount.TroopCount
getDefenseThroughWater gameMap players countryId =
    case findCountryOwner countryId players of
        Just playerId ->
            let
                countriesReachableThroughWater : List Map.CountryId
                countriesReachableThroughWater =
                    Map.getCountriesThatCanReachCountryThroughWater gameMap countryId

                defenderCountriesNeighboringWater : List Map.CountryId
                defenderCountriesNeighboringWater =
                    countriesReachableThroughWater
                        |> filterCountriesOwnedBy players playerId

                defenderCountriesNeighoboringWaterWithPort : List Map.CountryId
                defenderCountriesNeighoboringWaterWithPort =
                    defenderCountriesNeighboringWater
                        |> filterCountriesWithPort players
                        |> List.filter (\country -> country /= countryId)
            in
            defenderCountriesNeighoboringWaterWithPort
                |> List.foldl
                    (\(Map.CountryId countryWithPortId) result ->
                        case getTroopCountForCountry (Map.CountryId countryWithPortId) players of
                            Just troopCount ->
                                result |> Dict.insert countryWithPortId troopCount

                            Nothing ->
                                result
                    )
                    Dict.empty

        Nothing ->
            Dict.empty


getIsBeingMovedFrom : PlayerTurn -> Map.CountryId -> Bool
getIsBeingMovedFrom currentPlayerTurn countryId =
    case currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected fromCountryId _) _ ->
            fromCountryId == countryId

        _ ->
            False


getPlayerName : PlayerId -> Players -> Maybe String
getPlayerName playerId players =
    getPlayer playerId players
        |> Maybe.map .name


isCountryOwnedByPlayer : PlayerId -> Map.CountryId -> Players -> Bool
isCountryOwnedByPlayer playerId countryId players =
    case getPlayer playerId players of
        Just currentPlayer ->
            case getTroopCount countryId currentPlayer.countryTroopCounts of
                Just _ ->
                    True

                Nothing ->
                    False

        Nothing ->
            -- TODO: Handle this error case
            False


isCountryReachableFromOtherCountry : Map.CountryId -> Map.CountryId -> PlayerId -> Dict.Dict String Map.Country -> Players -> Bool
isCountryReachableFromOtherCountry fromCountryId toCountryId playerId countries players =
    case Map.getCountry fromCountryId countries of
        Just fromCountry ->
            case toCountryId of
                Map.CountryId toId ->
                    if Set.member toId fromCountry.neighboringCountries then
                        True

                    else if toCountryId /= fromCountryId then
                        case ( getCountryHasPort fromCountryId players, Map.getCountry toCountryId countries ) of
                            ( Just hasPort, Just toCountry ) ->
                                if hasPort then
                                    Set.size (Set.intersect fromCountry.neighboringBodiesOfWater toCountry.neighboringBodiesOfWater) > 0

                                else
                                    False

                            _ ->
                                -- shouldn't happen
                                False

                    else
                        False

        Nothing ->
            False


nextPlayer : Players -> PlayerId -> PlayerId
nextPlayer players (PlayerId currentPlayerId) =
    remainderBy (Dict.size players) currentPlayerId + 1 |> PlayerId


nextPlayerCheckForDeadPlayers : Players -> PlayerId -> PlayerId
nextPlayerCheckForDeadPlayers players currentPlayerId =
    -- This doesn't work during capitol placement because nobody will have a capitol except player 1 after player 1 places their capitol
    let
        nextPlayerId =
            currentPlayerId |> nextPlayer players
    in
    case getPlayer nextPlayerId players of
        Just newCurrentPlayer ->
            case newCurrentPlayer |> .capitolStatus of
                Capitol _ ->
                    nextPlayerId

                NoCapitol ->
                    nextPlayerId |> nextPlayerCheckForDeadPlayers players

        Nothing ->
            currentPlayerId


neutralCountryColor : Colors.Color
neutralCountryColor =
    Colors.gray


playerHasMoreThanOneCountry : Players -> PlayerId -> Bool
playerHasMoreThanOneCountry players playerId =
    getPlayer playerId players
        |> Maybe.map (\player -> Dict.size player.countryTroopCounts > 1)
        |> Maybe.withDefault False


playerTurnToPlayerId : PlayerTurn -> PlayerId
playerTurnToPlayerId (PlayerTurn _ playerId) =
    playerId


removePlayerCountry : Map.CountryId -> Game -> Result Error Game
removePlayerCountry (Map.CountryId countryId) activeGame =
    -- Make this return result with error if dict lookup fails
    case findCountryOwner (Map.CountryId countryId) activeGame.players of
        Just playerId ->
            activeGame
                |> updatePlayersWithPlayer playerId
                    (\player ->
                        { player
                            | countryTroopCounts = player.countryTroopCounts |> Dict.remove countryId
                        }
                    )

        Nothing ->
            "Error finding country owner" |> Error |> Err


removeTroopCount : Map.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
removeTroopCount (Map.CountryId countryId) troopCounts =
    Dict.remove countryId troopCounts


takeCountryFromOpponent : Map.CountryId -> Game -> Result Error Game
takeCountryFromOpponent countryId activeGame =
    activeGame
        |> removePlayerCountry countryId
        |> Result.andThen
            (\updatedGame ->
                updatedGame |> updatePlayerTroopCountForCountry (getCurrentPlayer activeGame.currentPlayerTurn) countryId TroopCount.noTroops
            )


troopsPerCountryPerTurn : Int
troopsPerCountryPerTurn =
    1


updateForSuccessfulAttack : Game -> Game
updateForSuccessfulAttack activeGame =
    let
        currentPlayerId =
            activeGame.currentPlayerTurn |> playerTurnToPlayerId

        nextPlayerTurn =
            let
                capitolsRemaining =
                    activeGame.players
                        |> Dict.values
                        |> List.foldl
                            (\player capitols ->
                                case player.capitolStatus of
                                    Capitol capitolId ->
                                        capitolId :: capitols

                                    NoCapitol ->
                                        capitols
                            )
                            []
            in
            if List.length capitolsRemaining == 1 then
                PlayerTurn GameOver currentPlayerId

            else if playerHasMoreThanOneCountry activeGame.players currentPlayerId then
                PlayerTurn TroopMovement currentPlayerId

            else
                PlayerTurn TroopPlacement (currentPlayerId |> nextPlayerCheckForDeadPlayers activeGame.players)
    in
    { activeGame
        | currentPlayerTurn = nextPlayerTurn
    }


updatePlayer : PlayerId -> Player -> Players -> Players
updatePlayer (PlayerId playerId) player players =
    Dict.insert playerId player players


updatePlayerTroopCountForCountry : PlayerId -> Map.CountryId -> TroopCount.TroopCount -> Game -> Result Error Game
updatePlayerTroopCountForCountry playerId countryId troops activeGame =
    -- Make this return result with error if dict lookup fails
    activeGame
        |> updatePlayersWithPlayer playerId
            (\player ->
                { player
                    | countryTroopCounts =
                        player.countryTroopCounts
                            |> updateTroopCount countryId troops
                }
            )


updatePlayersWithPlayer : PlayerId -> (Player -> Player) -> Game -> Result Error Game
updatePlayersWithPlayer playerId toUpdatedPlayer activeGame =
    case getPlayer playerId activeGame.players of
        Just player ->
            Ok
                { activeGame
                    | players =
                        activeGame.players
                            |> updatePlayer playerId (toUpdatedPlayer player)
                }

        Nothing ->
            "some error" |> Error |> Err


updateTroopCount :
    Map.CountryId
    -> TroopCount.TroopCount
    -> Dict.Dict String TroopCount.TroopCount
    -> Dict.Dict String TroopCount.TroopCount
updateTroopCount (Map.CountryId countryId) troopCount troopCounts =
    Dict.insert countryId troopCount troopCounts
