module ActiveGame exposing
    ( ActiveGame
    , CapitolStatus(..)
    , CountryBorderHelperOutlineStatus(..)
    , Player
    , PlayerId(..)
    , PlayerTurn
    , canCurrentPlayerCancelTroopMovement
    , canCurrentPlayerPass
    , cancelMovingTroops
    , findCountryOwner
    , getAttackStrengthPerPlayer
    , getCountryAttackers
    , getCountryDefenders
    , getCountryDefenseStrength
    , getCountryHasPort
    , getCurrentPlayer
    , getDefaultColor,getSelectedCountryForTroopMovement
    , getPlayer
    , getPlayerColorFromPlayerTurn
    , getPlayerCountryAndTroopCounts
    , getPlayerTurnStageFromPlayerTurn
    , getTroopCount
    , getTroopCountForCountry
    , handleCountryMouseDown
    , handleCountryMouseOut
    , handleCountryMouseUpFromPlayer
    , isCountryAttacking
    , isCountryDefending
    , isCountryIdCapitol
    , makeCountryHelperOutlinesActive
    , pass
    , pixelsPerMapSquare
    , playerIdToString
    , playerTurnToString
    , start
    , stopShowingCountryHelperOutlines
    ,  troopsToMove
       -- , updateCountryToShowInfoFor

    , updateNumberOfTroopsToMove
    , waitingToShowCountryHelperOutlines
    )

import Color
import Dict
import GameMap
import Set
import TroopCount


type alias ActiveGame =
    { currentPlayerTurn : PlayerTurn
    , map : GameMap.GameMap
    , players : Dict.Dict Int Player
    , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
    , error : Maybe String
    , numberOfPlayers : Int
    , countryBorderHelperOutlines : CountryBorderHelperOutlineStatus
    }


type CountryBorderHelperOutlineStatus
    = CountryBorderHelperOutlineWaitingForDelay GameMap.CountryId
    | CountryBorderHelperOutlineInactive
    | CountryBorderHelperOutlineActive GameMap.CountryId


makeCountryHelperOutlinesActive activeGame =
    case activeGame.countryBorderHelperOutlines of
        CountryBorderHelperOutlineWaitingForDelay countryId ->
            { activeGame | countryBorderHelperOutlines = CountryBorderHelperOutlineActive countryId }

        _ ->
            activeGame


type PlayerId
    = PlayerId Int


type alias Player =
    { name : String
    , countryTroopCounts : Dict.Dict String TroopCount.TroopCount
    , capitolStatus : CapitolStatus
    , color : Color.Color
    , ports : Set.Set String
    }


type PlayerTurn
    = PlayerTurn PlayerTurnStage PlayerId


type PlayerTurnStage
    = CapitolPlacement
    | TroopPlacement
    | AttackAnnexOrPort
    | TroopMovement
    | TroopMovementFromSelected GameMap.CountryId String
    | GameOver


type CapitolStatus
    = NoCapitol
    | Capitol GameMap.CountryId (Set.Set ( Float, Float ))


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

getSelectedCountryForTroopMovement : ActiveGame -> Maybe GameMap.CountryId
getSelectedCountryForTroopMovement activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected selectedCountryId _) _ ->
            Just selectedCountryId
        _ -> Nothing


-- Settings


troopsPerCountryPerTurn : Int
troopsPerCountryPerTurn =
    1


defaultPlayerColors : Dict.Dict Int Color.Color
defaultPlayerColors =
    Dict.fromList
        [ ( 1, Color.darkGreen )
        , ( 3, Color.lightGreen )
        , ( 2, Color.lightYellow )
        , ( 5, Color.orange )
        , ( 4, Color.brown )
        , ( 6, Color.lightPurple )
        ]



-- Exposed


waitingToShowCountryHelperOutlines : ActiveGame -> Bool
waitingToShowCountryHelperOutlines activeGame =
    case activeGame.countryBorderHelperOutlines of
        CountryBorderHelperOutlineWaitingForDelay _ ->
            True

        _ ->
            False


cancelMovingTroops : ActiveGame -> ActiveGame
cancelMovingTroops activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn _ playerId ->
            { activeGame | currentPlayerTurn = PlayerTurn TroopMovement playerId }


canCurrentPlayerPass : ActiveGame -> Bool
canCurrentPlayerPass activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn playerTurnStage _ ->
            case playerTurnStage of
                TroopMovement ->
                    True

                AttackAnnexOrPort ->
                    True

                _ ->
                    False


canCurrentPlayerCancelTroopMovement : ActiveGame -> Bool
canCurrentPlayerCancelTroopMovement activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn playerTurnStage _ ->
            case playerTurnStage of
                TroopMovementFromSelected _ _ ->
                    True

                _ ->
                    False


getCurrentPlayer : ActiveGame -> PlayerId
getCurrentPlayer activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn _ playerId ->
            playerId


isCountryDefending : ActiveGame -> GameMap.CountryId -> GameMap.CountryId -> Bool
isCountryDefending activeGame countryToDefend (GameMap.CountryId countryThatMightDefend) =
    let
        countryDefense =
            getCountryDefenders activeGame countryToDefend

        defendingCountries =
            Dict.keys countryDefense.neighboringCountryDefense ++ Dict.keys countryDefense.neighboringThroughWaterDefense
    in
    defendingCountries |> Set.fromList |> Set.member countryThatMightDefend


isCountryAttacking : ActiveGame -> GameMap.CountryId -> GameMap.CountryId -> Bool
isCountryAttacking activeGame countryToDefend (GameMap.CountryId countryThatMightDefend) =
    let
        countryAttackers =
            getCountryAttackers activeGame countryToDefend

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


getCountryDefenders : ActiveGame -> GameMap.CountryId -> CountryDefenders
getCountryDefenders activeGame countryId =
    let
        playerId : PlayerId
        playerId =
            case findCountryOwner countryId activeGame.players of
                Just ownerId ->
                    ownerId

                Nothing ->
                    PlayerId -1

        defendingCountryTroopCount : TroopCount.TroopCount
        defendingCountryTroopCount =
            case getTroopCountForCountry countryId activeGame.players of
                Just countryBeingAttackedTroopCount ->
                    countryBeingAttackedTroopCount

                Nothing ->
                    -- This shouldn't happen
                    TroopCount.noTroops

        neigboringCountryDefense : Dict.Dict String TroopCount.TroopCount
        neigboringCountryDefense =
            case GameMap.getCountry countryId activeGame.map.countries of
                Just countryBeingAttacked ->
                    countryBeingAttacked.neighboringCountries
                        |> Set.toList
                        |> List.map (\id -> GameMap.CountryId id)
                        |> List.foldl
                            (\(GameMap.CountryId neighboringCountryId) defense ->
                                if isCountryOwnedByPlayer playerId (GameMap.CountryId neighboringCountryId) activeGame.players then
                                    case getTroopCountForCountry (GameMap.CountryId neighboringCountryId) activeGame.players of
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
            getDefenseThroughWater activeGame countryId
                |> Dict.filter
                    (\throughWaterCountryId _ ->
                        case GameMap.getCountry countryId activeGame.map.countries of
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


getDefenseThroughWater : ActiveGame -> GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount
getDefenseThroughWater activeGame countryId =
    case findCountryOwner countryId activeGame.players of
        Just playerId ->
            let
                countriesReachableThroughWater : List GameMap.CountryId
                countriesReachableThroughWater =
                    GameMap.getCountriesThatCanReachCountryThroughWater activeGame.map countryId

                defenderCountriesNeighboringWater : List GameMap.CountryId
                defenderCountriesNeighboringWater =
                    countriesReachableThroughWater
                        |> filterCountriesOwnedBy activeGame.players playerId

                defenderCountriesNeighoboringWaterWithPort : List GameMap.CountryId
                defenderCountriesNeighoboringWaterWithPort =
                    defenderCountriesNeighboringWater
                        |> filterCountriesWithPort activeGame.players
                        |> List.filter (\country -> country /= countryId)
            in
            defenderCountriesNeighoboringWaterWithPort
                |> List.foldl
                    (\(GameMap.CountryId countryWithPortId) result ->
                        case getTroopCountForCountry (GameMap.CountryId countryWithPortId) activeGame.players of
                            Just troopCount ->
                                result |> Dict.insert countryWithPortId troopCount

                            Nothing ->
                                result
                    )
                    Dict.empty

        Nothing ->
            Dict.empty


getCountryDefenseStrength : ActiveGame -> GameMap.CountryId -> TroopCount.TroopCount
getCountryDefenseStrength activeGame countryId =
    let
        countryDefense =
            getCountryDefenders activeGame countryId

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


pixelsPerMapSquare : Int
pixelsPerMapSquare =
    100


findCountryOwner : GameMap.CountryId -> Dict.Dict Int Player -> Maybe PlayerId
findCountryOwner (GameMap.CountryId countryId) players =
    players
        |> Dict.foldl
            (\playerId player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countryTroopCounts
                            |> Maybe.map (\_ -> PlayerId playerId)
            )
            Nothing


getPlayerCountryAndTroopCounts : ActiveGame -> List { playerId : PlayerId, countryCount : Int, troopCount : TroopCount.TroopCount, isAlive : Bool }
getPlayerCountryAndTroopCounts activeGame =
    activeGame.players
        |> Dict.map
            (\playerId player ->
                case player.capitolStatus of
                    Capitol _ _ ->
                        { playerId = PlayerId playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = True
                        }

                    NoCapitol ->
                        { playerId = PlayerId playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = False || isCapitolPlacementTurn activeGame
                        }
            )
        |> Dict.values


isCapitolPlacementTurn : ActiveGame -> Bool
isCapitolPlacementTurn activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn CapitolPlacement _ ->
            True

        _ ->
            False


getTotalTroopCountForPlayer : Player -> TroopCount.TroopCount
getTotalTroopCountForPlayer player =
    player.countryTroopCounts
        |> Dict.values
        |> List.foldl (\troopCount result -> TroopCount.addTroopCounts troopCount result) TroopCount.noTroops


getPlayer : PlayerId -> Dict.Dict Int Player -> Maybe Player
getPlayer (PlayerId playerId) players =
    Dict.get playerId players


getTroopCount : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Maybe TroopCount.TroopCount
getTroopCount (GameMap.CountryId countryId) troopCounts =
    Dict.get countryId troopCounts


getTroopCountForCountry : GameMap.CountryId -> Dict.Dict Int Player -> Maybe TroopCount.TroopCount
getTroopCountForCountry countryId players =
    findCountryOwner countryId players
        |> Maybe.andThen (\playerId -> getPlayer playerId players)
        |> Maybe.andThen (\player -> getTroopCount countryId player.countryTroopCounts)


getDefaultColor : PlayerId -> Color.Color
getDefaultColor (PlayerId playerId) =
    case Dict.get playerId defaultPlayerColors of
        Just color ->
            color

        Nothing ->
            Color.black


getPlayerColorFromPlayerTurn : Dict.Dict Int Player -> PlayerTurn -> Color.Color
getPlayerColorFromPlayerTurn players playerTurn =
    case playerTurn of
        PlayerTurn _ playerId ->
            getPlayer playerId players
                |> Maybe.map
                    (\player ->
                        player.color
                    )
                |> Maybe.withDefault Color.black


getPlayerTurnStageFromPlayerTurn : PlayerTurn -> PlayerTurnStage
getPlayerTurnStageFromPlayerTurn playerTurn =
    case playerTurn of
        PlayerTurn playerTurnStage _ ->
            playerTurnStage


stopShowingCountryHelperOutlines : ActiveGame -> ActiveGame
stopShowingCountryHelperOutlines activeGame =
    { activeGame | countryBorderHelperOutlines = CountryBorderHelperOutlineInactive }


handleCountryMouseUpFromPlayer : GameMap.CountryId -> ActiveGame -> ActiveGame
handleCountryMouseUpFromPlayer clickedCountryId activeGame =
    let
        updatedActiveGame =
            case activeGame.countryBorderHelperOutlines of
                CountryBorderHelperOutlineActive _ ->
                    activeGame

                CountryBorderHelperOutlineInactive ->
                    activeGame

                CountryBorderHelperOutlineWaitingForDelay countryToShowInfoForId ->
                    if clickedCountryId == countryToShowInfoForId then
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
                                        { activeGame | error = Nothing }

                    else
                        activeGame
    in
    { updatedActiveGame | countryBorderHelperOutlines = CountryBorderHelperOutlineInactive }


handleCountryMouseDown : GameMap.CountryId -> ActiveGame -> ActiveGame
handleCountryMouseDown countryId activeGame =
    { activeGame | countryBorderHelperOutlines = CountryBorderHelperOutlineWaitingForDelay countryId }


handleCountryMouseOut : GameMap.CountryId -> ActiveGame -> ActiveGame
handleCountryMouseOut mouseOutCountryId activeGame =
    case activeGame.countryBorderHelperOutlines of
        CountryBorderHelperOutlineWaitingForDelay countryId ->
            if countryId == mouseOutCountryId then
                { activeGame | countryBorderHelperOutlines = CountryBorderHelperOutlineInactive }

            else
                activeGame

        _ ->
            activeGame


isCountryIdCapitol : PlayerId -> GameMap.CountryId -> Dict.Dict Int Player -> Maybe Bool
isCountryIdCapitol playerId countryId players =
    getPlayer playerId players
        |> Maybe.map
            (\player ->
                case player.capitolStatus of
                    Capitol capitolId _ ->
                        capitolId == countryId

                    NoCapitol ->
                        False
            )


numberOfTroopsToPlace : PlayerId -> Dict.Dict Int Player -> TroopCount.TroopCount
numberOfTroopsToPlace playerId players =
    case getPlayer playerId players of
        Just player ->
            TroopCount.numberOfTroopsToPlace (Dict.size player.countryTroopCounts) troopsPerCountryPerTurn

        Nothing ->
            -- TODO : Propogate error
            TroopCount.noTroops


playerIdToString : PlayerId -> String
playerIdToString (PlayerId playerId) =
    String.fromInt playerId


start : GameMap.GameMap -> Int -> Dict.Dict String TroopCount.TroopCount -> ActiveGame
start map numberOfPlayers neutralTroopCounts =
    { map = map
    , players =
        List.range 1 numberOfPlayers
            |> List.map
                (\playerId ->
                    ( playerId
                    , { countryTroopCounts = Dict.empty
                      , name = "Player " ++ String.fromInt playerId
                      , capitolStatus = NoCapitol
                      , color = getDefaultColor (PlayerId playerId)
                      , ports = Set.empty
                      }
                    )
                )
            |> Dict.fromList
    , currentPlayerTurn = PlayerTurn CapitolPlacement (PlayerId 1)
    , error = Nothing
    , numberOfPlayers = numberOfPlayers
    , neutralCountryTroops = neutralTroopCounts
    , countryBorderHelperOutlines = CountryBorderHelperOutlineInactive
    }



-- Not exposed


addPortForPlayer : GameMap.CountryId -> Player -> Player
addPortForPlayer (GameMap.CountryId countryId) player =
    { player | ports = player.ports |> Set.insert countryId }


attackAnnexOrPort : GameMap.CountryId -> PlayerId -> ActiveGame -> ActiveGame
attackAnnexOrPort clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame of
        OccupiedByCurrentPlayer _ ->
            attemptToBuildPort currentPlayerId clickedCountryId activeGame

        OccupiedByOpponent opponentPlayerId ->
            attemptToAttackCountry opponentPlayerId clickedCountryId activeGame

        Unoccupied ->
            attemptToAnnexCountry currentPlayerId clickedCountryId activeGame


attemptTroopMovement : GameMap.CountryId -> GameMap.CountryId -> String -> ActiveGame -> ActiveGame
attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame =
    case getCountryStatus clickedCountryId activeGame of
        OccupiedByCurrentPlayer playerCountryToTroopCount ->
            case String.toInt numberOfTroopsToMoveString of
                Just numberOfTroopsToMove ->
                    if isCountryReachableFromOtherCountry fromCountryId clickedCountryId (getCurrentPlayer activeGame) activeGame.map.countries activeGame.players then
                        let
                            fromCountryTroopCount =
                                case getPlayer (getCurrentPlayer activeGame) activeGame.players of
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

                            updatedGame =
                                activeGame
                                    |> updatePlayerTroopCountForCountry (getCurrentPlayer activeGame) fromCountryId (TroopCount.subtractTroopCounts allowedNumberOfTroopsToMove fromCountryTroopCount)
                                    |> updatePlayerTroopCountForCountry (getCurrentPlayer activeGame) clickedCountryId (TroopCount.addTroopCounts playerCountryToTroopCount allowedNumberOfTroopsToMove)
                        in
                        { updatedGame
                            | currentPlayerTurn = PlayerTurn TroopPlacement (getCurrentPlayer activeGame |> nextPlayerCheckForDeadPlayers activeGame.players)
                            , error = Nothing
                        }

                    else
                        { activeGame | error = Just "You can't move troops between those countries" }

                Nothing ->
                    { activeGame | error = Just "Number of troops must be a number" }

        _ ->
            { activeGame | error = Just "You must move troops to your own country" }


attemptToPlaceCapitol : GameMap.CountryId -> PlayerId -> ActiveGame -> ActiveGame
attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame =
    case GameMap.getCountry clickedCountryId activeGame.map.countries of
        Just clickedCountry ->
            case getCountryStatus clickedCountryId activeGame of
                OccupiedByCurrentPlayer _ ->
                    { activeGame | error = Just "Error: Somehow you are placing a second capitol" }

                OccupiedByOpponent _ ->
                    { activeGame | error = Just "You must select an unoccuppied country" }

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
                                        , capitolStatus = Capitol clickedCountryId (GameMap.capitolDotsCoordinates clickedCountry.coordinates pixelsPerMapSquare)
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
                            { activeGame
                                | players = updatedPlayers
                                , neutralCountryTroops = destroyTroops clickedCountryId activeGame.neutralCountryTroops
                                , currentPlayerTurn = nextPlayerTurn
                                , error = Nothing
                            }

                        Nothing ->
                            { activeGame | error = Just "Something bad happened" }

        _ ->
            { activeGame
                | error = Just "Something bad happened"
            }


attemptTroopPlacement : GameMap.CountryId -> PlayerId -> TroopCount.TroopCount -> ActiveGame -> ActiveGame
attemptTroopPlacement clickedCountryId currentPlayerId troopsToPlace activeGame =
    case getCountryStatus clickedCountryId activeGame of
        OccupiedByCurrentPlayer clickedCountryTroopCount ->
            let
                updatedGame =
                    activeGame |> updatePlayerTroopCountForCountry currentPlayerId clickedCountryId (TroopCount.addTroopCounts clickedCountryTroopCount troopsToPlace)
            in
            { updatedGame
                | currentPlayerTurn = PlayerTurn AttackAnnexOrPort currentPlayerId
                , error = Nothing
            }

        OccupiedByOpponent _ ->
            { activeGame | error = Just "You must put troops in your own country" }

        Unoccupied ->
            { activeGame | error = Just "You must put troops in your own country" }


attemptToBuildPort : PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
attemptToBuildPort currentPlayerId clickedCountryId activeGame =
    case getTroopCountForCountry clickedCountryId activeGame.players of
        Just _ ->
            buildPort currentPlayerId clickedCountryId activeGame

        Nothing ->
            { activeGame | error = Just "You can't build a port in a country you don't own" }


getAttackStrengthPerPlayer : ActiveGame -> GameMap.CountryId -> Dict.Dict Int TroopCount.TroopCount
getAttackStrengthPerPlayer activeGame countryId =
    getCountryAttackers activeGame countryId
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


getCountryAttackers : ActiveGame -> GameMap.CountryId -> CountryAttackers
getCountryAttackers activeGame countryId =
    let
        neighborCountriesByPlayer : List ( PlayerId, GameMap.CountryId )
        neighborCountriesByPlayer =
            case GameMap.getCountry countryId activeGame.map.countries of
                Just country ->
                    country.neighboringCountries
                        |> Set.foldl
                            (\neighborCountry result ->
                                case findCountryOwner (GameMap.CountryId neighborCountry) activeGame.players of
                                    Just neighborId ->
                                        ( neighborId, GameMap.CountryId neighborCountry ) :: result

                                    Nothing ->
                                        result
                            )
                            []

                Nothing ->
                    []

        neighborCountryTroopCountsByPlayer : List ( PlayerId, GameMap.CountryId, TroopCount.TroopCount )
        neighborCountryTroopCountsByPlayer =
            neighborCountriesByPlayer
                |> List.map
                    (\( playerId, neighborCountryId ) ->
                        ( playerId
                        , neighborCountryId
                        , getTroopCountForCountry neighborCountryId activeGame.players |> Maybe.withDefault TroopCount.noTroops
                          -- TODO
                        )
                    )

        neighboringCountryAttackers : Dict.Dict Int (Dict.Dict String TroopCount.TroopCount)
        neighboringCountryAttackers =
            neighborCountryTroopCountsByPlayer
                |> List.foldl
                    (\( PlayerId playerId, GameMap.CountryId neighborCountryId, troopCount ) result ->
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

        countriesReachableThroughWater : List GameMap.CountryId
        countriesReachableThroughWater =
            GameMap.getCountriesThatCanReachCountryThroughWater activeGame.map countryId

        attackerCountriesNeighoboringWater : List GameMap.CountryId
        attackerCountriesNeighoboringWater =
            countriesReachableThroughWater

        attackerCountriesNeighoboringWaterWithPort : List GameMap.CountryId
        attackerCountriesNeighoboringWaterWithPort =
            attackerCountriesNeighoboringWater
                |> filterCountriesWithPort activeGame.players

        waterNeighborCountriesByPlayer : List ( PlayerId, GameMap.CountryId )
        waterNeighborCountriesByPlayer =
            attackerCountriesNeighoboringWaterWithPort
                |> List.foldl
                    (\waterNeighborCountry result ->
                        case findCountryOwner waterNeighborCountry activeGame.players of
                            Just neighborId ->
                                ( neighborId, waterNeighborCountry ) :: result

                            Nothing ->
                                result
                    )
                    []

        waterNeighborCountriesByPlayerTroopCounts : List ( PlayerId, GameMap.CountryId, TroopCount.TroopCount )
        waterNeighborCountriesByPlayerTroopCounts =
            waterNeighborCountriesByPlayer
                |> List.map
                    (\( playerId, neighborCountryId ) ->
                        ( playerId
                        , neighborCountryId
                        , getTroopCountForCountry neighborCountryId activeGame.players |> Maybe.withDefault TroopCount.noTroops
                          -- TODO
                        )
                    )

        waterNeighborAttackers : Dict.Dict Int (Dict.Dict String TroopCount.TroopCount)
        waterNeighborAttackers =
            waterNeighborCountriesByPlayerTroopCounts
                |> List.foldl
                    (\( PlayerId playerId, GameMap.CountryId neighborCountryId, troopCount ) result ->
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
    activeGame.players
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



-- getWaterAttackStrength : PlayerId -> GameMap.CountryId -> ActiveGame -> TroopCount.TroopCount
-- getWaterAttackStrength attackerId countryBeingAttackedId activeGame =
--     let
--         countriesReachableThroughWater : List GameMap.CountryId
--         countriesReachableThroughWater =
--             GameMap.getCountriesThatCanReachCountryThroughWater activeGame.map countryBeingAttackedId
--         attackerCountriesNeighoboringWater : List GameMap.CountryId
--         attackerCountriesNeighoboringWater =
--             countriesReachableThroughWater
--                 |> filterCountriesOwnedBy activeGame.players attackerId
--         attackerCountriesNeighoboringWaterWithPort : List GameMap.CountryId
--         attackerCountriesNeighoboringWaterWithPort =
--             attackerCountriesNeighoboringWater
--                 |> filterCountriesWithPort activeGame.players
--         attackTroopCount : TroopCount.TroopCount
--         attackTroopCount =
--             attackerCountriesNeighoboringWaterWithPort
--                 |> countAllTroops activeGame.players
--     in
--     attackTroopCount |> TroopCount.acrossWater


attackResult : GameMap.CountryId -> ActiveGame -> AttackResult
attackResult clickedCountryId activeGame =
    case findCountryOwner clickedCountryId activeGame.players of
        Just opponentPlayerId ->
            let
                countryAttackers =
                    getAttackStrengthPerPlayer activeGame clickedCountryId

                currentPlayerId =
                    getCurrentPlayer activeGame

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
                    getCountryDefenseStrength activeGame clickedCountryId

                remainingTroops =
                    getTroopCountForCountry clickedCountryId activeGame.players
                        |> Maybe.withDefault TroopCount.noTroops
                        -- TODO
                        |> TroopCount.addTroopCounts defenseStrength
                        |> TroopCount.subtractTroopCounts attackStrength
            in
            if TroopCount.canAttack attackStrength defenseStrength then
                if TroopCount.hasTroops remainingTroops then
                    OpponentCountryLosesTroops remainingTroops

                else
                    case isCountryIdCapitol opponentPlayerId clickedCountryId activeGame.players of
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



-- case findCountryOwner clickedCountryId activeGame.players of
--     Just opponentPlayerId ->
--         case getTroopCountForCountry clickedCountryId activeGame.players of
--             Just opponentTroopCount ->
--                 let
--                     landAttackStrength =
--                         getLandAttackStrength clickedCountryId (getCurrentPlayer activeGame) activeGame
--                     waterAttackStrength =
--                         getWaterAttackStrength (getCurrentPlayer activeGame) clickedCountryId activeGame
--                     defenseStrength =
--                         getCountryDefenseStrength activeGame clickedCountryId
--                     attackStrength =
--                         landAttackStrength |> TroopCount.addTroopCounts waterAttackStrength
--                     remainingTroops =
--                         opponentTroopCount
--                             |> TroopCount.addTroopCounts defenseStrength
--                             |> TroopCount.subtractTroopCounts attackStrength
--                 in
--                 if TroopCount.canAttack attackStrength defenseStrength then
--                     if TroopCount.hasTroops remainingTroops then
--                         OpponentCountryLosesTroops remainingTroops
--                     else
--                         case isCountryIdCapitol opponentPlayerId clickedCountryId activeGame.players of
--                             Just isCapitol ->
--                                 if isCapitol then
--                                     OpponentEliminated
--                                 else
--                                     CurrentPlayerAcquiresOpponentCountry
--                             Nothing ->
--                                 AttackResultError "Error checking if capitol"
--                 else
--                     NotEnoughTroopsToAttack attackStrength defenseStrength
--             Nothing ->
--                 AttackResultError "Opponent does not own country"
--     Nothing ->
--         AttackResultError "No owner"


attemptSelectTroopMovementFromCountry : GameMap.CountryId -> PlayerId -> ActiveGame -> ActiveGame
attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame of
        OccupiedByCurrentPlayer troopCount ->
            if TroopCount.hasTroops troopCount then
                { activeGame
                    | currentPlayerTurn =
                        PlayerTurn (TroopMovementFromSelected clickedCountryId (TroopCount.toString troopCount)) currentPlayerId
                }
                    |> clearError

            else
                { activeGame | error = Just "Select a country with troops" }

        _ ->
            { activeGame | error = Just "You must move troops from your own country" }


attemptToAnnexCountry : PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
attemptToAnnexCountry currentPlayerId clickedCountryId activeGame =
    if canAnnexCountry activeGame.map currentPlayerId activeGame.players clickedCountryId then
        let
            neutralTroopCount =
                getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

            updatedGame =
                updatePlayerTroopCountForCountry currentPlayerId clickedCountryId neutralTroopCount activeGame
        in
        { updatedGame
            | currentPlayerTurn = PlayerTurn TroopMovement currentPlayerId
            , neutralCountryTroops = removeTroopCount clickedCountryId activeGame.neutralCountryTroops
        }
            |> clearError

    else
        { activeGame
            | error = Just "You can't annex that country"
        }


attemptToAttackCountry : PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
attemptToAttackCountry opponentPlayerId clickedCountryId activeGame =
    case attackResult clickedCountryId activeGame of
        OpponentCountryLosesTroops remainingTroops ->
            activeGame
                |> updatePlayerTroopCountForCountry opponentPlayerId clickedCountryId remainingTroops
                |> updateForSuccessfulAttack

        OpponentEliminated ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> destroyPlayer opponentPlayerId
                |> updateForSuccessfulAttack

        CurrentPlayerAcquiresOpponentCountry ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> updateForSuccessfulAttack

        NotEnoughTroopsToAttack attackStrength defenseStrength ->
            { activeGame
                | error = Just ("Not enough to attack: attack strength = " ++ TroopCount.toString attackStrength ++ ", defense strength = " ++ TroopCount.toString defenseStrength)
            }

        AttackResultError errorMessage ->
            { activeGame
                | error = Just errorMessage
            }


countAllTroops : Dict.Dict Int Player -> List GameMap.CountryId -> TroopCount.TroopCount
countAllTroops players countries =
    countries
        |> List.foldl
            (\countryId totalTroopCount ->
                case getTroopCountForCountry countryId players of
                    Just troopCount ->
                        TroopCount.addTroopCounts totalTroopCount troopCount

                    Nothing ->
                        totalTroopCount
            )
            TroopCount.noTroops


filterCountriesOwnedBy : Dict.Dict Int Player -> PlayerId -> List GameMap.CountryId -> List GameMap.CountryId
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
        (\(GameMap.CountryId countryId) result ->
            if Set.member countryId countriesOwnedByPlayer then
                GameMap.CountryId countryId :: result

            else
                result
        )
        []
        countryIds


filterCountriesWithPort : Dict.Dict Int Player -> List GameMap.CountryId -> List GameMap.CountryId
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


getLandAttackStrength : GameMap.CountryId -> PlayerId -> ActiveGame -> TroopCount.TroopCount
getLandAttackStrength countryBeingAttackedId playerId activeGame =
    case GameMap.getCountry countryBeingAttackedId activeGame.map.countries of
        Just countryBeingAttacked ->
            countryBeingAttacked.neighboringCountries
                |> Set.toList
                |> List.map (\id -> GameMap.CountryId id)
                |> List.foldl
                    (\neighboringCountryId attack ->
                        case getCountryStatus neighboringCountryId activeGame of
                            OccupiedByCurrentPlayer neighboringPlayerCountryTroopCount ->
                                TroopCount.addTroopCounts attack neighboringPlayerCountryTroopCount

                            _ ->
                                attack
                    )
                    TroopCount.noTroops

        Nothing ->
            -- This shouldn't happen
            TroopCount.noTroops


playerHasMoreThanOneCountry : Dict.Dict Int Player -> PlayerId -> Bool
playerHasMoreThanOneCountry players playerId =
    getPlayer playerId players
        |> Maybe.map (\player -> Dict.size player.countryTroopCounts > 1)
        |> Maybe.withDefault False


buildPort : PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
buildPort playerId countryId activeGame =
    -- We already had to check that the player owned this country before so no need to do that here
    case GameMap.isCountryNeighboringWater countryId activeGame.map.countries of
        Just isNeighboringWater ->
            if isNeighboringWater then
                case getCountryHasPort countryId activeGame.players of
                    Just hasPort ->
                        if hasPort then
                            { activeGame | error = Just "This country already has a port" }

                        else
                            let
                                updated =
                                    activeGame
                                        |> updatePlayersWithPlayer playerId (addPortForPlayer countryId)

                                nextPlayerTurn =
                                    if playerHasMoreThanOneCountry activeGame.players playerId then
                                        PlayerTurn TroopMovement playerId

                                    else
                                        PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                            in
                            { updated
                                | currentPlayerTurn = nextPlayerTurn
                                , error = Nothing
                            }

                    Nothing ->
                        { activeGame | error = Just "Error while building port" }

            else
                { activeGame | error = Just "A country must be next to water to build a port" }

        Nothing ->
            { activeGame | error = Just "Error checking if country borders water" }


canAnnexCountry : GameMap.GameMap -> PlayerId -> Dict.Dict Int Player -> GameMap.CountryId -> Bool
canAnnexCountry gameMap playerId players countryIdToAnnex =
    -- We already know the country is unoccuppied from an earlier check so just make sure it is reachable from one of the current players countries
    case getPlayer playerId players of
        Just player ->
            player.countryTroopCounts
                |> Dict.foldl
                    (\playerCountryId _ isReachable ->
                        isReachable || isCountryReachableFromOtherCountry (GameMap.CountryId playerCountryId) countryIdToAnnex playerId gameMap.countries players
                    )
                    False

        Nothing ->
            -- This should never happen
            False


clearError : ActiveGame -> ActiveGame
clearError activeGame =
    { activeGame | error = Nothing }


destroyPlayer : PlayerId -> ActiveGame -> ActiveGame
destroyPlayer playerId players =
    -- Make this return result with error if dict lookup fails
    players
        |> updatePlayersWithPlayer
            playerId
            (\player ->
                { player | capitolStatus = NoCapitol, countryTroopCounts = Dict.empty }
            )


destroyTroops : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
destroyTroops (GameMap.CountryId countryId) neutralTroopCounts =
    Dict.remove countryId neutralTroopCounts


getCountryHasPort : GameMap.CountryId -> Dict.Dict Int Player -> Maybe Bool
getCountryHasPort (GameMap.CountryId countryId) players =
    findCountryOwner (GameMap.CountryId countryId) players
        |> Maybe.andThen
            (\playerId ->
                getPlayer playerId players
                    |> Maybe.map .ports
                    |> Maybe.map
                        (Set.member countryId)
            )


getCountryStatus : GameMap.CountryId -> ActiveGame -> CountryStatus
getCountryStatus countryId activeGame =
    case getPlayer (getCurrentPlayer activeGame) activeGame.players of
        Just currentPlayer ->
            case getTroopCount countryId currentPlayer.countryTroopCounts of
                Just troopCount ->
                    OccupiedByCurrentPlayer troopCount

                Nothing ->
                    case
                        activeGame.players
                            |> Dict.foldl
                                (\playerId player result ->
                                    case result of
                                        Just _ ->
                                            result

                                        Nothing ->
                                            case getTroopCount countryId player.countryTroopCounts of
                                                Just _ ->
                                                    Just (OccupiedByOpponent (PlayerId playerId))

                                                Nothing ->
                                                    Nothing
                                )
                                Nothing
                    of
                        Just occupiedByOppenent ->
                            occupiedByOppenent

                        Nothing ->
                            Unoccupied

        Nothing ->
            Unoccupied


getPlayerName : PlayerId -> Dict.Dict Int Player -> Maybe String
getPlayerName playerId players =
    getPlayer playerId players
        |> Maybe.map .name


isCountryOwnedByPlayer : PlayerId -> GameMap.CountryId -> Dict.Dict Int Player -> Bool
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


isCountryReachableFromOtherCountry : GameMap.CountryId -> GameMap.CountryId -> PlayerId -> Dict.Dict String GameMap.Country -> Dict.Dict Int Player -> Bool
isCountryReachableFromOtherCountry fromCountryId toCountryId playerId countries players =
    case GameMap.getCountry fromCountryId countries of
        Just fromCountry ->
            case toCountryId of
                GameMap.CountryId toId ->
                    if Set.member toId fromCountry.neighboringCountries then
                        True

                    else if toCountryId /= fromCountryId then
                        case ( getCountryHasPort fromCountryId players, GameMap.getCountry toCountryId countries ) of
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


nextPlayer : Dict.Dict Int Player -> PlayerId -> PlayerId
nextPlayer players (PlayerId currentPlayerId) =
    remainderBy (Dict.size players) currentPlayerId + 1 |> PlayerId


nextPlayerCheckForDeadPlayers : Dict.Dict Int Player -> PlayerId -> PlayerId
nextPlayerCheckForDeadPlayers players currentPlayerId =
    -- This doesn't work during capitol placement because nobody will have a capitol except player 1 after player 1 places their capitol
    let
        nextPlayerId =
            currentPlayerId |> nextPlayer players
    in
    case getPlayer nextPlayerId players of
        Just newCurrentPlayer ->
            case newCurrentPlayer |> .capitolStatus of
                Capitol _ _ ->
                    nextPlayerId

                NoCapitol ->
                    nextPlayerId |> nextPlayerCheckForDeadPlayers players

        Nothing ->
            currentPlayerId


pass : ActiveGame -> ActiveGame
pass activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected _ _) playerId ->
            { activeGame | currentPlayerTurn = PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }
                |> clearError

        PlayerTurn TroopMovement playerId ->
            { activeGame | currentPlayerTurn = PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }
                |> clearError

        PlayerTurn AttackAnnexOrPort playerId ->
            { activeGame
                | currentPlayerTurn =
                    if playerHasMoreThanOneCountry activeGame.players playerId then
                        PlayerTurn TroopMovement playerId

                    else
                        PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
            }
                |> clearError

        _ ->
            activeGame


playerTurnToString : Dict.Dict Int Player -> PlayerTurn -> String
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


playerTurnToPlayerId : PlayerTurn -> PlayerId
playerTurnToPlayerId (PlayerTurn _ playerId) =
    playerId


removePlayerCountry : GameMap.CountryId -> ActiveGame -> ActiveGame
removePlayerCountry (GameMap.CountryId countryId) activeGame =
    -- Make this return result with error if dict lookup fails
    case findCountryOwner (GameMap.CountryId countryId) activeGame.players of
        Just playerId ->
            activeGame
                |> updatePlayersWithPlayer playerId
                    (\player ->
                        { player
                            | countryTroopCounts = player.countryTroopCounts |> Dict.remove countryId
                        }
                    )

        Nothing ->
            activeGame


removeTroopCount : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
removeTroopCount (GameMap.CountryId countryId) troopCounts =
    Dict.remove countryId troopCounts


takeCountryFromOpponent : GameMap.CountryId -> ActiveGame -> ActiveGame
takeCountryFromOpponent countryId activeGame =
    activeGame
        |> removePlayerCountry countryId
        |> updatePlayerTroopCountForCountry (getCurrentPlayer activeGame) countryId TroopCount.noTroops


troopsToMove : ActiveGame -> Maybe String
troopsToMove activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected _ troops) _ ->
            Just troops

        _ ->
            Nothing


updateForSuccessfulAttack : ActiveGame -> ActiveGame
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
                                    Capitol capitolId _ ->
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
        , error = Nothing
    }


updateNumberOfTroopsToMove : String -> ActiveGame -> ActiveGame
updateNumberOfTroopsToMove numberOfTroopsToMoveString activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected countryId _) currentPlayerId ->
            { activeGame
                | currentPlayerTurn =
                    currentPlayerId |> PlayerTurn (TroopMovementFromSelected countryId numberOfTroopsToMoveString)
            }

        _ ->
            activeGame


updatePlayer : PlayerId -> Player -> Dict.Dict Int Player -> Dict.Dict Int Player
updatePlayer (PlayerId playerId) player players =
    Dict.insert playerId player players


updatePlayerTroopCountForCountry : PlayerId -> GameMap.CountryId -> TroopCount.TroopCount -> ActiveGame -> ActiveGame
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


updatePlayersWithPlayer : PlayerId -> (Player -> Player) -> ActiveGame -> ActiveGame
updatePlayersWithPlayer playerId toUpdatedPlayer activeGame =
    case getPlayer playerId activeGame.players of
        Just player ->
            { activeGame
                | players =
                    activeGame.players
                        |> updatePlayer playerId (toUpdatedPlayer player)
            }

        Nothing ->
            { activeGame | error = Just "some error" }


updateTroopCount : GameMap.CountryId -> TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
updateTroopCount (GameMap.CountryId countryId) troopCount troopCounts =
    Dict.insert countryId troopCount troopCounts
