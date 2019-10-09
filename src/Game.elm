module Game exposing
    ( CountryToRender
    , Error
    , Game
    , Id(..)
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
    , getPlayerColorFromPlayerTurn
    , getPlayerCountryAndTroopCounts, save
    , getTroopCount
    , getTroopCountForCountry
    , idToString
    , isCountryAttacking
    , isCountryDefending
    , isCountryIdCapitol
    , pass
    , playerUrlParser
    , updateNumberOfTroopsToMove
    , urlParser
    )

import Api.Enum.PlayerTurnStage
import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Game
import Api.Object.Player
import Api.Object.PlayerTurn
import Api.Query
import Colors
import Country
import Dict
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet exposing (SelectionSet)
import Map
import Player
import PlayerTurn
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


playerUrlParser : Url.Parser.Parser (Player.Id -> a) a
playerUrlParser =
    Url.Parser.custom "PLAYERID" (\playerId -> playerId |> String.toInt |> Maybe.map Player.Id)


type alias Game =
    { id : Id
    , currentPlayerTurn : PlayerTurn.PlayerTurn
    , map : Map.Map
    , players : Player.Players
    , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
    , numberOfPlayers : Int
    }


type alias CountryToRender =
    { id : Country.Id
    , color : Colors.Color
    , troopCount : TroopCount.TroopCount
    , center : Country.ScaledPoint
    , polygonPoints : List Country.ScaledPoint
    , capitolDots : Maybe (Set.Set Country.ScaledPoint)
    , canBeClicked : Bool
    , isBeingMovedFrom : Bool
    , portSegments : Maybe (Set.Set ( Country.ScaledPoint, Country.ScaledPoint ))
    }


type Error
    = Error String


type Id
    = Id String


create : String -> Int -> Dict.Dict String TroopCount.TroopCount -> (RemoteData.RemoteData (Graphql.Http.Error Id) Id -> msg) -> Cmd msg
create selectedMapId numberOfPlayers neutralTroopCounts toMsg =
    let
        playerTurnInput : Api.InputObject.PlayerTurnInput
        playerTurnInput =
            { playerId = "1"
            , playerTurnStage = Api.Enum.PlayerTurnStage.CapitolPlacement
            }

        neutralCountryTroops : List Api.InputObject.CountryTroopCountsInput
        neutralCountryTroops =
            neutralTroopCounts |> TroopCount.troopCountsInput

        input : Api.Mutation.CreateGameRequiredArguments
        input =
            { newGame =
                Api.InputObject.buildNewGameInput
                    { mapId = selectedMapId
                    , players = Player.createDefaultPlayers numberOfPlayers |> Player.input
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


save : Game -> (RemoteData.RemoteData (Graphql.Http.Error Id) Id -> msg) -> Cmd msg
save game toMsg =
    let
        input : Api.Mutation.SaveGameRequiredArguments
        input =
            { game =
                Api.InputObject.buildGameInput
                    { id = game.id |> idToString
                    , mapId = game.map.id |> Map.idToString
                    , players = game.players |> Player.input
                    , neutralCountryTroops = game.neutralCountryTroops |> TroopCount.troopCountsInput
                    , numberOfPlayers = game.numberOfPlayers
                    , playerTurn = game.currentPlayerTurn |> PlayerTurn.playerTurnInput
                    }
            }

        gameSelectionSet : Graphql.SelectionSet.SelectionSet Id Api.Object.Game
        gameSelectionSet =
            Api.Object.Game.id |> Graphql.SelectionSet.map Id
    in
    Api.Mutation.saveGame input gameSelectionSet
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)



-- Debug.todo ""


get : Id -> (RemoteData.RemoteData (Graphql.Http.Error Game) Game -> msg) -> Cmd msg
get (Id id) toMsg =
    let
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
                            , players = players |> Player.playerSelectionSetsToPlayers
                            , neutralCountryTroops = neutralCountryTroops |> Dict.fromList
                            , numberOfPlayers = numberOfPlayers
                            }
                    in
                    activeGame
                )
                Api.Object.Game.id
                (Api.Object.Game.map Map.mapSelection)
                (Api.Object.Game.playerTurn PlayerTurn.selectionSet)
                (Api.Object.Game.players Player.playerSelection)
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


countryClicked : Country.Id -> Game -> Result Error Game
countryClicked clickedCountryId activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn playerTurnStage currentPlayerId ->
            case playerTurnStage of
                PlayerTurn.CapitolPlacement ->
                    attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame

                PlayerTurn.TroopPlacement ->
                    attemptTroopPlacement clickedCountryId currentPlayerId (Player.numberOfTroopsToPlace currentPlayerId activeGame.players) activeGame

                PlayerTurn.AttackAnnexOrPort ->
                    attackAnnexOrPort clickedCountryId currentPlayerId activeGame

                PlayerTurn.TroopMovement ->
                    attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame

                PlayerTurn.TroopMovementFromSelected fromCountryId numberOfTroopsToMoveString ->
                    attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame

                PlayerTurn.GameOver ->
                    Ok activeGame


getAttackStrengthPerPlayer : Map.Map -> Player.Players -> Country.Id -> Dict.Dict Int TroopCount.TroopCount
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


getCountryAttackers : Map.Map -> Player.Players -> Country.Id -> CountryAttackers
getCountryAttackers gameMap players countryId =
    let
        neighborCountriesByPlayer : List ( Player.Id, Country.Id )
        neighborCountriesByPlayer =
            case Country.getCountry countryId gameMap.countries of
                Just country ->
                    country.neighboringCountries
                        |> Set.foldl
                            (\neighborCountry result ->
                                case findCountryOwner (Country.Id neighborCountry) players of
                                    Just neighborId ->
                                        ( neighborId, Country.Id neighborCountry ) :: result

                                    Nothing ->
                                        result
                            )
                            []

                Nothing ->
                    []

        neighborCountryTroopCountsByPlayer : List ( Player.Id, Country.Id, TroopCount.TroopCount )
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
                    (\( Player.Id playerId, Country.Id neighborCountryId, troopCount ) result ->
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

        countriesReachableThroughWater : List Country.Id
        countriesReachableThroughWater =
            Map.getCountriesThatCanReachCountryThroughWater gameMap countryId

        attackerCountriesNeighoboringWater : List Country.Id
        attackerCountriesNeighoboringWater =
            countriesReachableThroughWater

        attackerCountriesNeighoboringWaterWithPort : List Country.Id
        attackerCountriesNeighoboringWaterWithPort =
            attackerCountriesNeighoboringWater
                |> filterCountriesWithPort players

        waterNeighborCountriesByPlayer : List ( Player.Id, Country.Id )
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

        waterNeighborCountriesByPlayerTroopCounts : List ( Player.Id, Country.Id, TroopCount.TroopCount )
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
                    (\( Player.Id playerId, Country.Id neighborCountryId, troopCount ) result ->
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


getCountriesToRender : Map.Map -> Player.Players -> PlayerTurn.PlayerTurn -> Dict.Dict String TroopCount.TroopCount -> Maybe (List CountryToRender)
getCountriesToRender gameMap players currentPlayerTurn neutralCountryTroops =
    gameMap.countries
        |> Country.scaledCountries ViewHelpers.pixelsPerMapSquare
        |> Dict.map
            (\countryId country ->
                let
                    countryOwnerAndTroopCount =
                        findCountryOwnerAndTroopCount (Country.Id countryId) players
                in
                case countryOwnerAndTroopCount of
                    Just ( countryOwnerId, troopCount ) ->
                        Player.getPlayer countryOwnerId players
                            |> Maybe.map
                                (\countryOwner ->
                                    { id = Country.Id countryId
                                    , troopCount = troopCount
                                    , center = country.center
                                    , polygonPoints = country.polygon
                                    , color = countryOwner.color
                                    , capitolDots =
                                        case countryOwner.capitolStatus of
                                            Player.Capitol (Country.Id capitolId) ->
                                                if capitolId == countryId then
                                                    Just country.coordinates

                                                else
                                                    Nothing

                                            Player.NoCapitol ->
                                                Nothing
                                    , canBeClicked = getCountryCanBeClicked currentPlayerTurn players gameMap (Country.Id countryId)
                                    , isBeingMovedFrom = getIsBeingMovedFrom currentPlayerTurn (Country.Id countryId)
                                    , portSegments =
                                        getCountryHasPort (Country.Id countryId) players
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
                            { id = Country.Id countryId
                            , troopCount = getTroopCount (Country.Id countryId) neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops
                            , center = country.center
                            , color = neutralCountryColor
                            , polygonPoints = country.polygon
                            , capitolDots = Nothing
                            , canBeClicked = getCountryCanBeClicked currentPlayerTurn players gameMap (Country.Id countryId)
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
        PlayerTurn.PlayerTurn _ playerId ->
            { activeGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId }


isCountryDefending : Map.Map -> Player.Players -> Country.Id -> Country.Id -> Bool
isCountryDefending gameMap players countryToDefend (Country.Id countryThatMightDefend) =
    let
        countryDefense =
            getCountryDefenders players gameMap countryToDefend

        defendingCountries =
            Dict.keys countryDefense.neighboringCountryDefense ++ Dict.keys countryDefense.neighboringThroughWaterDefense
    in
    defendingCountries |> Set.fromList |> Set.member countryThatMightDefend


isCountryAttacking : Map.Map -> Player.Players -> Country.Id -> Country.Id -> Bool
isCountryAttacking gameMap players countryToDefend (Country.Id countryThatMightDefend) =
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


getCountryDefenders : Player.Players -> Map.Map -> Country.Id -> CountryDefenders
getCountryDefenders players gameMap countryId =
    let
        playerId : Player.Id
        playerId =
            case findCountryOwner countryId players of
                Just ownerId ->
                    ownerId

                Nothing ->
                    Player.Id -1

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
            case Country.getCountry countryId gameMap.countries of
                Just countryBeingAttacked ->
                    countryBeingAttacked.neighboringCountries
                        |> Set.toList
                        |> List.map (\id -> Country.Id id)
                        |> List.foldl
                            (\(Country.Id neighboringCountryId) defense ->
                                if isCountryOwnedByPlayer playerId (Country.Id neighboringCountryId) players then
                                    case getTroopCountForCountry (Country.Id neighboringCountryId) players of
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
                        case Country.getCountry countryId gameMap.countries of
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


getCountryDefenseStrength : Map.Map -> Player.Players -> Country.Id -> TroopCount.TroopCount
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


findCountryOwner : Country.Id -> Player.Players -> Maybe Player.Id
findCountryOwner countryId players =
    findCountryOwnerAndTroopCount countryId players
        |> Maybe.map Tuple.first


getCountryHasPort : Country.Id -> Player.Players -> Maybe Bool
getCountryHasPort (Country.Id countryId) players =
    findCountryOwner (Country.Id countryId) players
        |> Maybe.andThen
            (\playerId ->
                Player.getPlayer playerId players
                    |> Maybe.map .ports
                    |> Maybe.map
                        (Set.member countryId)
            )


getPlayerCountryAndTroopCounts :
    { players : Player.Players, currentPlayerTurn : PlayerTurn.PlayerTurn }
    -> List { playerId : Player.Id, countryCount : Int, troopCount : TroopCount.TroopCount, isAlive : Bool }
getPlayerCountryAndTroopCounts { players, currentPlayerTurn } =
    players
        |> Dict.map
            (\playerId player ->
                case player.capitolStatus of
                    Player.Capitol _ ->
                        { playerId = Player.Id playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = True
                        }

                    Player.NoCapitol ->
                        { playerId = Player.Id playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = False || PlayerTurn.isCapitolPlacementTurn currentPlayerTurn
                        }
            )
        |> Dict.values


getTotalTroopCountForPlayer : Player.Player -> TroopCount.TroopCount
getTotalTroopCountForPlayer player =
    player.countryTroopCounts
        |> Dict.values
        |> List.foldl (\troopCount result -> TroopCount.addTroopCounts troopCount result) TroopCount.noTroops


getTroopCount : Country.Id -> Dict.Dict String TroopCount.TroopCount -> Maybe TroopCount.TroopCount
getTroopCount (Country.Id countryId) troopCounts =
    Dict.get countryId troopCounts


getTroopCountForCountry : Country.Id -> Player.Players -> Maybe TroopCount.TroopCount
getTroopCountForCountry countryId players =
    findCountryOwner countryId players
        |> Maybe.andThen (\playerId -> Player.getPlayer playerId players)
        |> Maybe.andThen (\player -> getTroopCount countryId player.countryTroopCounts)


getPlayerColorFromPlayerTurn : Player.Players -> PlayerTurn.PlayerTurn -> Colors.Color
getPlayerColorFromPlayerTurn players playerTurn =
    case playerTurn of
        PlayerTurn.PlayerTurn _ playerId ->
            Player.getPlayer playerId players
                |> Maybe.map
                    (\player ->
                        player.color
                    )
                |> Maybe.withDefault Colors.black


isCountryIdCapitol : Player.Id -> Country.Id -> Player.Players -> Maybe Bool
isCountryIdCapitol playerId countryId players =
    Player.getPlayer playerId players
        |> Maybe.map
            (\player ->
                case player.capitolStatus of
                    Player.Capitol capitolId ->
                        capitolId == countryId

                    Player.NoCapitol ->
                        False
            )


pass : Game -> Result Error Game
pass activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected _ _) playerId ->
            Ok { activeGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }

        PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId ->
            Ok { activeGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }

        PlayerTurn.PlayerTurn PlayerTurn.AttackAnnexOrPort playerId ->
            Ok
                { activeGame
                    | currentPlayerTurn =
                        if playerHasMoreThanOneCountry activeGame.players playerId then
                            PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId

                        else
                            PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                }

        _ ->
            "Can't pass" |> Error |> Err


updateNumberOfTroopsToMove : String -> Game -> Game
updateNumberOfTroopsToMove numberOfTroopsToMoveString activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected countryId _) currentPlayerId ->
            { activeGame
                | currentPlayerTurn =
                    currentPlayerId |> PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected countryId numberOfTroopsToMoveString)
            }

        _ ->
            activeGame



------- LOCAL


type CountryStatus
    = Unoccupied
    | OccupiedByOpponent Player.Id
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


attackAnnexOrPort : Country.Id -> Player.Id -> Game -> Result Error Game
attackAnnexOrPort clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer _ ->
            attemptToBuildPort currentPlayerId clickedCountryId activeGame

        OccupiedByOpponent opponentPlayerId ->
            attemptToAttackCountry opponentPlayerId clickedCountryId activeGame

        Unoccupied ->
            attemptToAnnexCountry currentPlayerId clickedCountryId activeGame


attemptTroopMovement : Country.Id -> Country.Id -> String -> Game -> Result Error Game
attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer playerCountryToTroopCount ->
            case String.toInt numberOfTroopsToMoveString of
                Just numberOfTroopsToMove ->
                    if isCountryReachableFromOtherCountry fromCountryId clickedCountryId activeGame.map.countries activeGame.players then
                        let
                            fromCountryTroopCount =
                                case Player.getPlayer (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) activeGame.players of
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
                                    |> updatePlayerTroopCountForCountry (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) fromCountryId (TroopCount.subtractTroopCounts allowedNumberOfTroopsToMove fromCountryTroopCount)
                                    |> Result.andThen (updatePlayerTroopCountForCountry (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) clickedCountryId (TroopCount.addTroopCounts playerCountryToTroopCount allowedNumberOfTroopsToMove))
                        in
                        updatedGameResult
                            |> Result.map
                                (\updatedGame ->
                                    { updatedGame
                                        | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn |> nextPlayerCheckForDeadPlayers activeGame.players)
                                    }
                                )

                    else
                        "You can't move troops between those countries" |> Error |> Err

                Nothing ->
                    "Number of troops must be a number" |> Error |> Err

        _ ->
            "You must move troops to your own country" |> Error |> Err


attemptToPlaceCapitol : Country.Id -> Player.Id -> Game -> Result Error Game
attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame =
    case Country.getCountry clickedCountryId activeGame.map.countries of
        Just clickedCountry ->
            case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
                OccupiedByCurrentPlayer _ ->
                    "Error: Somehow you are placing a second capitol" |> Error |> Err

                OccupiedByOpponent _ ->
                    "You must select an unoccuppied country" |> Error |> Err

                Unoccupied ->
                    case Player.getPlayer currentPlayerId activeGame.players of
                        Just currentPlayer ->
                            let
                                neutralTroopCount =
                                    getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

                                updatedPlayer =
                                    { currentPlayer
                                        | countryTroopCounts =
                                            updateTroopCount clickedCountryId neutralTroopCount currentPlayer.countryTroopCounts
                                        , capitolStatus = Player.Capitol clickedCountryId

                                        -- , capitolStatus = Capitol clickedCountryId (Map.capitolDotsCoordinates clickedCountry.coordinates ViewHelpers.pixelsPerMapSquare)
                                    }

                                updatedPlayers =
                                    updatePlayer currentPlayerId updatedPlayer activeGame.players

                                nextPlayerId =
                                    case currentPlayerId of
                                        Player.Id id ->
                                            Player.Id (remainderBy (Dict.size updatedPlayers) id + 1)

                                nextPlayerTurn =
                                    case currentPlayerId of
                                        Player.Id id ->
                                            if id == Dict.size updatedPlayers then
                                                PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement nextPlayerId

                                            else
                                                PlayerTurn.PlayerTurn PlayerTurn.CapitolPlacement nextPlayerId
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


attemptTroopPlacement : Country.Id -> Player.Id -> TroopCount.TroopCount -> Game -> Result Error Game
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
                        { updatedGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.AttackAnnexOrPort currentPlayerId }
                    )

        OccupiedByOpponent _ ->
            "You must put troops in your own country" |> Error |> Err

        Unoccupied ->
            "You must put troops in your own country" |> Error |> Err


attemptToBuildPort : Player.Id -> Country.Id -> Game -> Result Error Game
attemptToBuildPort currentPlayerId clickedCountryId activeGame =
    case getTroopCountForCountry clickedCountryId activeGame.players of
        Just _ ->
            buildPort currentPlayerId clickedCountryId activeGame

        Nothing ->
            "You can't build a port in a country you don't own" |> Error |> Err


attackResult : Country.Id -> Map.Map -> Player.Players -> PlayerTurn.PlayerTurn -> AttackResult
attackResult clickedCountryId gameMap players currentPlayerTurn =
    case findCountryOwner clickedCountryId players of
        Just opponentPlayerId ->
            let
                countryAttackers =
                    getAttackStrengthPerPlayer gameMap players clickedCountryId

                currentPlayerId =
                    PlayerTurn.getCurrentPlayer currentPlayerTurn

                currentPlayerIdInt =
                    case currentPlayerId of
                        Player.Id playerId ->
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


attemptSelectTroopMovementFromCountry : Country.Id -> Player.Id -> Game -> Result Error Game
attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer troopCount ->
            if TroopCount.hasTroops troopCount then
                Ok
                    { activeGame
                        | currentPlayerTurn =
                            PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected clickedCountryId (TroopCount.toString troopCount)) currentPlayerId
                    }

            else
                "Select a country with troops" |> Error |> Err

        _ ->
            "You must move troops from your own country" |> Error |> Err


attemptToAnnexCountry : Player.Id -> Country.Id -> Game -> Result Error Game
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
                        | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopMovement currentPlayerId
                        , neutralCountryTroops = removeTroopCount clickedCountryId activeGame.neutralCountryTroops
                    }
                )

    else
        "You can't annex that country" |> Error |> Err


attemptToAttackCountry : Player.Id -> Country.Id -> Game -> Result Error Game
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


filterCountriesOwnedBy : Player.Players -> Player.Id -> List Country.Id -> List Country.Id
filterCountriesOwnedBy players playerId countryIds =
    let
        countriesOwnedByPlayer : Set.Set String
        countriesOwnedByPlayer =
            case Player.getPlayer playerId players of
                Just player ->
                    player.countryTroopCounts |> Dict.keys |> Set.fromList

                Nothing ->
                    Set.empty
    in
    List.foldl
        (\(Country.Id countryId) result ->
            if Set.member countryId countriesOwnedByPlayer then
                Country.Id countryId :: result

            else
                result
        )
        []
        countryIds


buildPort : Player.Id -> Country.Id -> Game -> Result Error Game
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
                                        |> updatePlayersWithPlayer playerId (Player.addPort countryId)

                                nextPlayerTurn =
                                    if playerHasMoreThanOneCountry activeGame.players playerId then
                                        PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId

                                    else
                                        PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                            in
                            updatedGameResult
                                |> Result.map (\updated -> { updated | currentPlayerTurn = nextPlayerTurn })

                    Nothing ->
                        "Error while building port" |> Error |> Err

            else
                "A country must be next to water to build a port" |> Error |> Err

        Nothing ->
            "Error checking if country borders water" |> Error |> Err


canAnnexCountry : Map.Map -> Player.Id -> Player.Players -> Country.Id -> Bool
canAnnexCountry gameMap playerId players countryIdToAnnex =
    -- We already know the country is unoccuppied from an earlier check so just make sure it is reachable from one of the current players countries
    case Player.getPlayer playerId players of
        Just player ->
            player.countryTroopCounts
                |> Dict.foldl
                    (\playerCountryId _ isReachable ->
                        isReachable || isCountryReachableFromOtherCountry (Country.Id playerCountryId) countryIdToAnnex gameMap.countries players
                    )
                    False

        Nothing ->
            -- This should never happen
            False


destroyPlayer : Player.Id -> Game -> Result Error Game
destroyPlayer playerId activeGame =
    -- Make this return result with error if dict lookup fails
    activeGame
        |> updatePlayersWithPlayer
            playerId
            (\player ->
                { player | capitolStatus = Player.NoCapitol, countryTroopCounts = Dict.empty }
            )


destroyTroops : Country.Id -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
destroyTroops (Country.Id countryId) neutralTroopCounts =
    Dict.remove countryId neutralTroopCounts


filterCountriesWithPort : Player.Players -> List Country.Id -> List Country.Id
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


findCountryOwnerAndTroopCount : Country.Id -> Player.Players -> Maybe ( Player.Id, TroopCount.TroopCount )
findCountryOwnerAndTroopCount (Country.Id countryId) players =
    players
        |> Dict.foldl
            (\playerId player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countryTroopCounts
                            |> Maybe.map (\troopCount -> ( Player.Id playerId, troopCount ))
            )
            Nothing


getCountryCanBeClicked : PlayerTurn.PlayerTurn -> Player.Players -> Map.Map -> Country.Id -> Bool
getCountryCanBeClicked currentPlayerTurn players gameMap countryId =
    case currentPlayerTurn of
        PlayerTurn.PlayerTurn playerTurnStatus currentPlayerId ->
            case playerTurnStatus of
                PlayerTurn.CapitolPlacement ->
                    case findCountryOwner countryId players of
                        Just _ ->
                            False

                        Nothing ->
                            True

                PlayerTurn.TroopPlacement ->
                    case findCountryOwner countryId players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                PlayerTurn.AttackAnnexOrPort ->
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

                PlayerTurn.TroopMovement ->
                    case findCountryOwner countryId players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                PlayerTurn.TroopMovementFromSelected fromCountryId _ ->
                    if isCountryReachableFromOtherCountry fromCountryId countryId gameMap.countries players then
                        case Player.getPlayer (PlayerTurn.getCurrentPlayer currentPlayerTurn) players of
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


getCountryStatus : Country.Id -> Player.Players -> PlayerTurn.PlayerTurn -> CountryStatus
getCountryStatus countryId players currentPlayerTurn =
    case Player.getPlayer (PlayerTurn.getCurrentPlayer currentPlayerTurn) players of
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
                                                |> Maybe.map (\_ -> OccupiedByOpponent (Player.Id playerId))
                                )
                                Nothing
                    of
                        Just occupiedByOppenent ->
                            occupiedByOppenent

                        Nothing ->
                            Unoccupied

        Nothing ->
            Unoccupied


getDefenseThroughWater : Map.Map -> Player.Players -> Country.Id -> Dict.Dict String TroopCount.TroopCount
getDefenseThroughWater gameMap players countryId =
    case findCountryOwner countryId players of
        Just playerId ->
            let
                countriesReachableThroughWater : List Country.Id
                countriesReachableThroughWater =
                    Map.getCountriesThatCanReachCountryThroughWater gameMap countryId

                defenderCountriesNeighboringWater : List Country.Id
                defenderCountriesNeighboringWater =
                    countriesReachableThroughWater
                        |> filterCountriesOwnedBy players playerId

                defenderCountriesNeighoboringWaterWithPort : List Country.Id
                defenderCountriesNeighoboringWaterWithPort =
                    defenderCountriesNeighboringWater
                        |> filterCountriesWithPort players
                        |> List.filter (\country -> country /= countryId)
            in
            defenderCountriesNeighoboringWaterWithPort
                |> List.foldl
                    (\(Country.Id countryWithPortId) result ->
                        case getTroopCountForCountry (Country.Id countryWithPortId) players of
                            Just troopCount ->
                                result |> Dict.insert countryWithPortId troopCount

                            Nothing ->
                                result
                    )
                    Dict.empty

        Nothing ->
            Dict.empty


getIsBeingMovedFrom : PlayerTurn.PlayerTurn -> Country.Id -> Bool
getIsBeingMovedFrom currentPlayerTurn countryId =
    case currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected fromCountryId _) _ ->
            fromCountryId == countryId

        _ ->
            False


isCountryOwnedByPlayer : Player.Id -> Country.Id -> Player.Players -> Bool
isCountryOwnedByPlayer playerId countryId players =
    case Player.getPlayer playerId players of
        Just currentPlayer ->
            case getTroopCount countryId currentPlayer.countryTroopCounts of
                Just _ ->
                    True

                Nothing ->
                    False

        Nothing ->
            -- TODO: Handle this error case
            False


isCountryReachableFromOtherCountry : Country.Id -> Country.Id -> Dict.Dict String Country.Country -> Player.Players -> Bool
isCountryReachableFromOtherCountry fromCountryId toCountryId countries players =
    case Country.getCountry fromCountryId countries of
        Just fromCountry ->
            case toCountryId of
                Country.Id toId ->
                    if Set.member toId fromCountry.neighboringCountries then
                        True

                    else if toCountryId /= fromCountryId then
                        case ( getCountryHasPort fromCountryId players, Country.getCountry toCountryId countries ) of
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


nextPlayer : Player.Players -> Player.Id -> Player.Id
nextPlayer players (Player.Id currentPlayerId) =
    remainderBy (Dict.size players) currentPlayerId + 1 |> Player.Id


nextPlayerCheckForDeadPlayers : Player.Players -> Player.Id -> Player.Id
nextPlayerCheckForDeadPlayers players currentPlayerId =
    -- This doesn't work during capitol placement because nobody will have a capitol except player 1 after player 1 places their capitol
    let
        nextPlayerId =
            currentPlayerId |> nextPlayer players
    in
    case Player.getPlayer nextPlayerId players of
        Just newCurrentPlayer ->
            case newCurrentPlayer |> .capitolStatus of
                Player.Capitol _ ->
                    nextPlayerId

                Player.NoCapitol ->
                    nextPlayerId |> nextPlayerCheckForDeadPlayers players

        Nothing ->
            currentPlayerId


neutralCountryColor : Colors.Color
neutralCountryColor =
    Colors.gray


playerHasMoreThanOneCountry : Player.Players -> Player.Id -> Bool
playerHasMoreThanOneCountry players playerId =
    Player.getPlayer playerId players
        |> Maybe.map (\player -> Dict.size player.countryTroopCounts > 1)
        |> Maybe.withDefault False


playerTurnToPlayerId : PlayerTurn.PlayerTurn -> Player.Id
playerTurnToPlayerId (PlayerTurn.PlayerTurn _ playerId) =
    playerId


removePlayerCountry : Country.Id -> Game -> Result Error Game
removePlayerCountry (Country.Id countryId) activeGame =
    -- Make this return result with error if dict lookup fails
    case findCountryOwner (Country.Id countryId) activeGame.players of
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


removeTroopCount : Country.Id -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
removeTroopCount (Country.Id countryId) troopCounts =
    Dict.remove countryId troopCounts


takeCountryFromOpponent : Country.Id -> Game -> Result Error Game
takeCountryFromOpponent countryId activeGame =
    activeGame
        |> removePlayerCountry countryId
        |> Result.andThen
            (\updatedGame ->
                updatedGame |> updatePlayerTroopCountForCountry (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) countryId TroopCount.noTroops
            )


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
                                    Player.Capitol capitolId ->
                                        capitolId :: capitols

                                    Player.NoCapitol ->
                                        capitols
                            )
                            []
            in
            if List.length capitolsRemaining == 1 then
                PlayerTurn.PlayerTurn PlayerTurn.GameOver currentPlayerId

            else if playerHasMoreThanOneCountry activeGame.players currentPlayerId then
                PlayerTurn.PlayerTurn PlayerTurn.TroopMovement currentPlayerId

            else
                PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (currentPlayerId |> nextPlayerCheckForDeadPlayers activeGame.players)
    in
    { activeGame
        | currentPlayerTurn = nextPlayerTurn
    }


updatePlayer : Player.Id -> Player.Player -> Player.Players -> Player.Players
updatePlayer (Player.Id playerId) player players =
    Dict.insert playerId player players


updatePlayerTroopCountForCountry : Player.Id -> Country.Id -> TroopCount.TroopCount -> Game -> Result Error Game
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


updatePlayersWithPlayer : Player.Id -> (Player.Player -> Player.Player) -> Game -> Result Error Game
updatePlayersWithPlayer playerId toUpdatedPlayer activeGame =
    case Player.getPlayer playerId activeGame.players of
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
    Country.Id
    -> TroopCount.TroopCount
    -> Dict.Dict String TroopCount.TroopCount
    -> Dict.Dict String TroopCount.TroopCount
updateTroopCount (Country.Id countryId) troopCount troopCounts =
    Dict.insert countryId troopCount troopCounts
