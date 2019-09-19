module ActiveGame exposing
    ( ActiveGame
    , CapitolStatus(..)
    , Player
    , PlayerId
    , PlayerTurn
    , canCurrentPlayerCancelTroopMovement
    , canCurrentPlayerPass
    , cancelMovingTroops
    , defaultScale
    , findCountryOwner
    , getCountryHasPort
    , getCurrentPlayer
    , getDefaultColor
    , getPlayer
    , getPlayerColorFromPlayerTurn
    , getPlayerCountryAndTroopCounts
    , getPlayerTurnStageFromPlayerTurn
    , getTroopCount
    , getTroopCountForPlayerCountry
    , handleCountryClickFromPlayer
    , isCountryIdCapitol
    , pass
    , playerIdToString
    , playerTurnToString
    , start
    , troopToMove
    , updateNumberOfTroopsToMove
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
    }


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


defaultScale : Int
defaultScale =
    12


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


getPlayerCountryAndTroopCounts : ActiveGame -> List ( PlayerId, Int, TroopCount.TroopCount )
getPlayerCountryAndTroopCounts activeGame =
    activeGame.players
        |> Dict.map
            (\playerId player ->
                ( PlayerId playerId, Dict.size player.countryTroopCounts, getTotalTroopCountForPlayer player )
            )
        |> Dict.values



-- todo move


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


getTroopCountForPlayerCountry : GameMap.CountryId -> PlayerId -> Dict.Dict Int Player -> Maybe TroopCount.TroopCount
getTroopCountForPlayerCountry countryId playerId players =
    getPlayer playerId players
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


handleCountryClickFromPlayer : GameMap.CountryId -> ActiveGame -> ActiveGame
handleCountryClickFromPlayer clickedCountryId activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn _ playerId ->
            case getPlayer playerId activeGame.players of
                Just _ ->
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
                                    attemptTroopMovement fromCountryId clickedCountryId currentPlayerId numberOfTroopsToMoveString activeGame

                                GameOver ->
                                    { activeGame | error = Nothing }

                Nothing ->
                    { activeGame | error = Just "Error handling click" }


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
    }



-- Not exposed


addPortForPlayer : GameMap.CountryId -> Player -> Player
addPortForPlayer (GameMap.CountryId countryId) player =
    { player | ports = player.ports |> Set.insert countryId }


attackAndDefenseStrength : GameMap.CountryId -> PlayerId -> PlayerId -> Dict.Dict Int Player -> Dict.Dict String GameMap.Country -> Dict.Dict String (Set.Set String) -> ( TroopCount.TroopCount, TroopCount.TroopCount )
attackAndDefenseStrength countryBeingAttackedId attackerId defenderId players countries bodyOfWaterNeighbors =
    let
        defendingCountryTroopCount =
            case getTroopCountForPlayerCountry countryBeingAttackedId defenderId players of
                Just countryBeingAttackedTroopCount ->
                    countryBeingAttackedTroopCount

                Nothing ->
                    -- This shouldn't happen
                    TroopCount.noTroops

        ( landAttackStrength, landDefenseStrength ) =
            attackAndDefenseStrengthLand countryBeingAttackedId attackerId defenderId players countries

        ( waterAttackStrength, waterDefenseStrength ) =
            attackAndDefenseStrengthWater countryBeingAttackedId attackerId defenderId players countries bodyOfWaterNeighbors
    in
    ( landAttackStrength |> TroopCount.addTroopCounts waterAttackStrength
    , landDefenseStrength
        |> TroopCount.addTroopCounts defendingCountryTroopCount
        |> TroopCount.addTroopCounts waterDefenseStrength
    )


attackAndDefenseStrengthLand : GameMap.CountryId -> PlayerId -> PlayerId -> Dict.Dict Int Player -> Dict.Dict String GameMap.Country -> ( TroopCount.TroopCount, TroopCount.TroopCount )
attackAndDefenseStrengthLand countryBeingAttackedId attackerId defenderId players countries =
    case GameMap.getCountry countryBeingAttackedId countries of
        Just countryBeingAttacked ->
            countryBeingAttacked.neighboringCountries
                |> Set.toList
                |> List.map (\id -> GameMap.CountryId id)
                |> List.foldl
                    (\neighboringCountryId ( attack, defense ) ->
                        case getCountryStatus neighboringCountryId attackerId players of
                            OccupiedByCurrentPlayer neighboringPlayerCountryTroopCount ->
                                ( TroopCount.addTroopCounts attack neighboringPlayerCountryTroopCount, defense )

                            OccupiedByOpponent neigborPlayerId ->
                                if neigborPlayerId == defenderId then
                                    case getTroopCountForPlayerCountry neighboringCountryId defenderId players of
                                        Just neighboringCountryTroopCount ->
                                            ( attack, TroopCount.addTroopCounts defense neighboringCountryTroopCount )

                                        Nothing ->
                                            -- This shouldn't happen
                                            ( attack, defense )

                                else
                                    ( attack, defense )

                            _ ->
                                ( attack, defense )
                    )
                    ( TroopCount.noTroops, TroopCount.noTroops )

        Nothing ->
            -- This shouldn't happen
            ( TroopCount.noTroops, TroopCount.noTroops )


attackAndDefenseStrengthWater : GameMap.CountryId -> PlayerId -> PlayerId -> Dict.Dict Int Player -> Dict.Dict String GameMap.Country -> Dict.Dict String (Set.Set String) -> ( TroopCount.TroopCount, TroopCount.TroopCount )
attackAndDefenseStrengthWater countryBeingAttackedId attackerId defenderId players countries bodyOfWaterNeighbors =
    let
        neighboringBodiesOfWater =
            case GameMap.getCountry countryBeingAttackedId countries of
                Just countryBeingAttacked ->
                    countryBeingAttacked.neighboringBodiesOfWater

                Nothing ->
                    -- TODO
                    Set.empty

        ( attackerCountriesNeighoboringWater, defenderCountriesNeighboringWater ) =
            neighboringBodiesOfWater
                |> Set.foldl
                    (\bodyOfWaterId ( attackerCountries, defenderCountries ) ->
                        case Dict.get bodyOfWaterId bodyOfWaterNeighbors of
                            Just countryIdsNeighboringWater ->
                                countryIdsNeighboringWater
                                    |> Set.foldl
                                        (\countryId ( innerAttack, innerDefense ) ->
                                            case findCountryOwner (GameMap.CountryId countryId) players of
                                                Just countryOwnerId ->
                                                    if countryOwnerId == attackerId then
                                                        ( Set.insert countryId innerAttack, innerDefense )

                                                    else if countryOwnerId == defenderId then
                                                        ( innerAttack, Set.insert countryId innerDefense )

                                                    else
                                                        ( innerAttack, innerDefense )

                                                Nothing ->
                                                    ( innerAttack, innerDefense )
                                        )
                                        ( attackerCountries, defenderCountries )

                            _ ->
                                ( attackerCountries, defenderCountries )
                    )
                    ( Set.empty, Set.empty )

        attackerCountriesNeighoboringWaterWithPort : Set.Set String
        attackerCountriesNeighoboringWaterWithPort =
            attackerCountriesNeighoboringWater
                |> Set.filter
                    (\attackCountryId ->
                        case getCountryHasPort attackerId (GameMap.CountryId attackCountryId) players of
                            Just hasPort ->
                                hasPort

                            _ ->
                                False
                    )

        defenderCountriesNeighoboringWaterWithPort : Set.Set String
        defenderCountriesNeighoboringWaterWithPort =
            defenderCountriesNeighboringWater
                |> Set.filter
                    (\defenseCountryId ->
                        case getCountryHasPort defenderId (GameMap.CountryId defenseCountryId) players of
                            Just hasPort ->
                                hasPort

                            _ ->
                                False
                    )

        attackTroopCount : TroopCount.TroopCount
        attackTroopCount =
            attackerCountriesNeighoboringWaterWithPort
                |> Set.foldl
                    (\countryId totalTroopCount ->
                        case getTroopCountForPlayerCountry (GameMap.CountryId countryId) attackerId players of
                            Just troopCount ->
                                TroopCount.addTroopCounts totalTroopCount troopCount

                            Nothing ->
                                totalTroopCount
                    )
                    TroopCount.noTroops

        defenderTroopCount : TroopCount.TroopCount
        defenderTroopCount =
            defenderCountriesNeighoboringWaterWithPort
                |> Set.foldl
                    (\countryId totalTroopCount ->
                        case getTroopCountForPlayerCountry (GameMap.CountryId countryId) defenderId players of
                            Just troopCount ->
                                TroopCount.addTroopCounts totalTroopCount troopCount

                            Nothing ->
                                totalTroopCount
                    )
                    TroopCount.noTroops
    in
    ( attackTroopCount |> TroopCount.acrossWater, defenderTroopCount |> TroopCount.acrossWater )


attackAnnexOrPort : GameMap.CountryId -> PlayerId -> ActiveGame -> ActiveGame
attackAnnexOrPort clickedCountryId currentPlayerId playingGameAttributes =
    case getCountryStatus clickedCountryId currentPlayerId playingGameAttributes.players of
        OccupiedByCurrentPlayer _ ->
            attemptToBuildPort currentPlayerId clickedCountryId playingGameAttributes

        OccupiedByOpponent opponentPlayerId ->
            attemptToAttackCountry currentPlayerId opponentPlayerId clickedCountryId playingGameAttributes

        Unoccupied ->
            attemptToAnnexCountry currentPlayerId clickedCountryId playingGameAttributes


attemptTroopMovement : GameMap.CountryId -> GameMap.CountryId -> PlayerId -> String -> ActiveGame -> ActiveGame
attemptTroopMovement fromCountryId clickedCountryId currentPlayerId numberOfTroopsToMoveString playingGameAttributes =
    case getCountryStatus clickedCountryId currentPlayerId playingGameAttributes.players of
        OccupiedByCurrentPlayer playerCountryToTroopCount ->
            case String.toInt numberOfTroopsToMoveString of
                Just numberOfTroopsToMove ->
                    if isCountryReachableFromOtherCountry fromCountryId clickedCountryId currentPlayerId playingGameAttributes.map.countries playingGameAttributes.players then
                        let
                            fromCountryTroopCount =
                                case getPlayer currentPlayerId playingGameAttributes.players of
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

                            updatedPlayers =
                                playingGameAttributes.players
                                    |> updatePlayerTroopCountForCountry currentPlayerId fromCountryId (TroopCount.subtractTroopCounts allowedNumberOfTroopsToMove fromCountryTroopCount)
                                    |> updatePlayerTroopCountForCountry currentPlayerId clickedCountryId (TroopCount.addTroopCounts playerCountryToTroopCount allowedNumberOfTroopsToMove)
                        in
                        { playingGameAttributes
                            | players = updatedPlayers
                            , currentPlayerTurn = PlayerTurn TroopPlacement (currentPlayerId |> nextPlayerCheckForDeadPlayers playingGameAttributes.players)
                            , error = Nothing
                        }

                    else
                        { playingGameAttributes | error = Just "You can't move troops between those countries" }

                Nothing ->
                    { playingGameAttributes | error = Just "Number of troops must be a number" }

        _ ->
            { playingGameAttributes | error = Just "You must move troops to your own country" }


attemptToPlaceCapitol : GameMap.CountryId -> PlayerId -> ActiveGame -> ActiveGame
attemptToPlaceCapitol clickedCountryId currentPlayerId playingGameAttributes =
    case GameMap.getCountry clickedCountryId playingGameAttributes.map.countries of
        Just clickedCountry ->
            case getCountryStatus clickedCountryId currentPlayerId playingGameAttributes.players of
                OccupiedByCurrentPlayer _ ->
                    { playingGameAttributes | error = Just "Error: Somehow you are placing a second capitol" }

                OccupiedByOpponent _ ->
                    { playingGameAttributes | error = Just "You must select an unoccuppied country" }

                Unoccupied ->
                    case getPlayer currentPlayerId playingGameAttributes.players of
                        Just currentPlayer ->
                            let
                                neutralTroopCount =
                                    getTroopCount clickedCountryId playingGameAttributes.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

                                updatedPlayer =
                                    { currentPlayer
                                        | countryTroopCounts =
                                            updateTroopCount clickedCountryId neutralTroopCount currentPlayer.countryTroopCounts
                                        , capitolStatus = Capitol clickedCountryId (GameMap.capitolDotsCoordinates clickedCountry.coordinates defaultScale)
                                    }

                                updatedPlayers =
                                    updatePlayer currentPlayerId updatedPlayer playingGameAttributes.players

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
                            { playingGameAttributes
                                | players = updatedPlayers
                                , neutralCountryTroops = destroyTroops clickedCountryId playingGameAttributes.neutralCountryTroops
                                , currentPlayerTurn = nextPlayerTurn
                                , error = Nothing
                            }

                        Nothing ->
                            { playingGameAttributes | error = Just "Something bad happened" }

        _ ->
            { playingGameAttributes
                | error = Just "Something bad happened"
            }


attemptTroopPlacement : GameMap.CountryId -> PlayerId -> TroopCount.TroopCount -> ActiveGame -> ActiveGame
attemptTroopPlacement clickedCountryId currentPlayerId troopsToPlace playingGameAttributes =
    case getCountryStatus clickedCountryId currentPlayerId playingGameAttributes.players of
        OccupiedByCurrentPlayer clickedCountryTroopCount ->
            let
                updatedPlayers =
                    playingGameAttributes.players |> updatePlayerTroopCountForCountry currentPlayerId clickedCountryId (TroopCount.addTroopCounts clickedCountryTroopCount troopsToPlace)
            in
            { playingGameAttributes
                | players = updatedPlayers
                , currentPlayerTurn = PlayerTurn AttackAnnexOrPort currentPlayerId
                , error = Nothing
            }

        OccupiedByOpponent _ ->
            { playingGameAttributes | error = Just "You must put troops in your own country" }

        Unoccupied ->
            { playingGameAttributes | error = Just "You must put troops in your own country" }


attemptToBuildPort : PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
attemptToBuildPort currentPlayerId clickedCountryId playingGameAttributes =
    case getTroopCountForPlayerCountry clickedCountryId currentPlayerId playingGameAttributes.players of
        Just _ ->
            buildPort currentPlayerId clickedCountryId playingGameAttributes

        Nothing ->
            { playingGameAttributes | error = Just "You can't build a port in a country you don't own" }


attackResult : PlayerId -> PlayerId -> GameMap.CountryId -> ActiveGame -> AttackResult
attackResult currentPlayerId opponentPlayerId clickedCountryId playingGameAttributes =
    case getTroopCountForPlayerCountry clickedCountryId opponentPlayerId playingGameAttributes.players of
        Just opponentTroopCount ->
            let
                ( attackStrength, defenseStrength ) =
                    attackAndDefenseStrength clickedCountryId currentPlayerId opponentPlayerId playingGameAttributes.players playingGameAttributes.map.countries playingGameAttributes.map.bodiesOfWater

                remainingTroops =
                    opponentTroopCount
                        |> TroopCount.addTroopCounts defenseStrength
                        |> TroopCount.subtractTroopCounts attackStrength
            in
            if TroopCount.canAttack attackStrength defenseStrength then
                if TroopCount.hasTroops remainingTroops then
                    OpponentCountryLosesTroops remainingTroops

                else
                    case isCountryIdCapitol opponentPlayerId clickedCountryId playingGameAttributes.players of
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
            AttackResultError "Opponent does not own country"


attemptSelectTroopMovementFromCountry : GameMap.CountryId -> PlayerId -> ActiveGame -> ActiveGame
attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId playingGameAttributes =
    case getCountryStatus clickedCountryId currentPlayerId playingGameAttributes.players of
        OccupiedByCurrentPlayer troopCount ->
            if TroopCount.hasTroops troopCount then
                { playingGameAttributes
                    | currentPlayerTurn =
                        PlayerTurn (TroopMovementFromSelected clickedCountryId (TroopCount.toString troopCount)) currentPlayerId
                    , error = Nothing
                }

            else
                { playingGameAttributes | error = Just "Select a country with troops" }

        _ ->
            { playingGameAttributes | error = Just "You must move troops from your own country" }


attemptToAnnexCountry : PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
attemptToAnnexCountry currentPlayerId clickedCountryId playingGameAttributes =
    if canAnnexCountry playingGameAttributes.map currentPlayerId playingGameAttributes.players clickedCountryId then
        let
            neutralTroopCount =
                getTroopCount clickedCountryId playingGameAttributes.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops
        in
        { playingGameAttributes
            | players = updatePlayerTroopCountForCountry currentPlayerId clickedCountryId neutralTroopCount playingGameAttributes.players
            , currentPlayerTurn = PlayerTurn TroopMovement currentPlayerId
            , neutralCountryTroops = removeTroopCount clickedCountryId playingGameAttributes.neutralCountryTroops
            , error = Nothing
        }

    else
        { playingGameAttributes
            | error = Just "You can't annex that country"
        }


attemptToAttackCountry : PlayerId -> PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
attemptToAttackCountry currentPlayerId opponentPlayerId clickedCountryId playingGameAttributes =
    case attackResult currentPlayerId opponentPlayerId clickedCountryId playingGameAttributes of
        OpponentCountryLosesTroops remainingTroops ->
            let
                updatedPlayers =
                    playingGameAttributes.players
                        |> updatePlayerTroopCountForCountry opponentPlayerId clickedCountryId remainingTroops
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
                | error = Just ("Not enough to attack: attack strength = " ++ TroopCount.toString attackStrength ++ ", defense strength = " ++ TroopCount.toString defenseStrength)
            }

        AttackResultError errorMessage ->
            { playingGameAttributes
                | error = Just errorMessage
            }


playerHasMoreThanOneCountry : Dict.Dict Int Player -> PlayerId -> Bool
playerHasMoreThanOneCountry players playerId =
    getPlayer playerId players
        |> Maybe.map (\player -> Dict.size player.countryTroopCounts > 1)
        |> Maybe.withDefault False


buildPort : PlayerId -> GameMap.CountryId -> ActiveGame -> ActiveGame
buildPort playerId countryId playingGameAttributes =
    -- We already had to check that the player owned this country before so no need to do that here
    case GameMap.isCountryNeighboringWater countryId playingGameAttributes.map.countries of
        Just isNeighboringWater ->
            if isNeighboringWater then
                case getCountryHasPort playerId countryId playingGameAttributes.players of
                    Just hasPort ->
                        if hasPort then
                            { playingGameAttributes | error = Just "This country already has a port" }

                        else
                            let
                                updated =
                                    playingGameAttributes
                                        |> updatePlayersWithPlayer playerId (addPortForPlayer countryId)

                                nextPlayerTurn =
                                    if playerHasMoreThanOneCountry playingGameAttributes.players playerId then
                                        PlayerTurn TroopMovement playerId

                                    else
                                        PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers playingGameAttributes.players)
                            in
                            { updated
                                | currentPlayerTurn = nextPlayerTurn
                                , error = Nothing
                            }

                    Nothing ->
                        { playingGameAttributes | error = Just "Error while building port" }

            else
                { playingGameAttributes | error = Just "A country must be next to water to build a port" }

        Nothing ->
            { playingGameAttributes | error = Just "Error checking if country borders water" }


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


destroyPlayer : PlayerId -> Dict.Dict Int Player -> Dict.Dict Int Player
destroyPlayer (PlayerId playerId) players =
    -- Make this return result with error if dict lookup fails
    case Dict.get playerId players of
        Just player ->
            players
                |> Dict.insert playerId { player | capitolStatus = NoCapitol, countryTroopCounts = Dict.empty }

        Nothing ->
            players


destroyTroops : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
destroyTroops (GameMap.CountryId countryId) neutralTroopCounts =
    Dict.remove countryId neutralTroopCounts


getCountryHasPort : PlayerId -> GameMap.CountryId -> Dict.Dict Int Player -> Maybe Bool
getCountryHasPort playerId (GameMap.CountryId countryId) players =
    getPlayer playerId players
        |> Maybe.map .ports
        |> Maybe.map
            (Set.member countryId)


getCountryStatus : GameMap.CountryId -> PlayerId -> Dict.Dict Int Player -> CountryStatus
getCountryStatus countryId currentPlayerId players =
    case getPlayer currentPlayerId players of
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
            -- This should never happen hopefully, but should return a result of sort instead
            Unoccupied


isCountryReachableFromOtherCountry : GameMap.CountryId -> GameMap.CountryId -> PlayerId -> Dict.Dict String GameMap.Country -> Dict.Dict Int Player -> Bool
isCountryReachableFromOtherCountry fromCountryId toCountryId playerId countries players =
    case GameMap.getCountry fromCountryId countries of
        Just fromCountry ->
            case toCountryId of
                GameMap.CountryId toId ->
                    if Set.member toId fromCountry.neighboringCountries then
                        True

                    else if toCountryId /= fromCountryId then
                        case ( getCountryHasPort playerId fromCountryId players, GameMap.getCountry toCountryId countries ) of
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
            case newCurrentPlayer.capitolStatus of
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
                    playerName ++ ": Choose an enemy country to attack, a neutral country to annex, or one of your ccountries bordering water to build a port"

                TroopMovement ->
                    playerName ++ ": Choose a country to move troops from or press the \"Pass\" button for no troop movement"

                TroopMovementFromSelected _ _ ->
                    playerName ++ ": Enter the number of troops to move and choose a destination or press the \"Pass\" button for no movement"

                GameOver ->
                    playerName ++ " has won the game!!!"

        Nothing ->
            -- TODO
            ""


getPlayerName : PlayerId -> Dict.Dict Int Player -> Maybe String
getPlayerName playerId players =
    getPlayer playerId players
        |> Maybe.map .name


playerTurnToPlayerId : PlayerTurn -> PlayerId
playerTurnToPlayerId (PlayerTurn _ playerId) =
    playerId


removePlayerCountry : GameMap.CountryId -> PlayerId -> Dict.Dict Int Player -> Dict.Dict Int Player
removePlayerCountry (GameMap.CountryId countryId) playerId players =
    -- Make this return result with error if dict lookup fails
    case getPlayer playerId players of
        Just player ->
            players
                |> updatePlayer
                    playerId
                    { player
                        | countryTroopCounts = player.countryTroopCounts |> Dict.remove countryId
                    }

        Nothing ->
            players


removeTroopCount : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
removeTroopCount (GameMap.CountryId countryId) troopCounts =
    Dict.remove countryId troopCounts


takeCountryFromOpponent : GameMap.CountryId -> PlayerId -> PlayerId -> Dict.Dict Int Player -> Dict.Dict Int Player
takeCountryFromOpponent countryId currentPlayerId opponentPlayerId players =
    players
        |> removePlayerCountry countryId opponentPlayerId
        |> updatePlayerTroopCountForCountry currentPlayerId countryId TroopCount.noTroops


troopToMove : ActiveGame -> Maybe String
troopToMove activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected _ troops) _ ->
            Just troops

        _ ->
            Nothing


updateForSuccessfulAttack : Dict.Dict Int Player -> ActiveGame -> ActiveGame
updateForSuccessfulAttack updatedPlayers playingGameAttributes =
    let
        currentPlayerId =
            playingGameAttributes.currentPlayerTurn |> playerTurnToPlayerId

        nextPlayerTurn =
            let
                capitolsRemaining =
                    updatedPlayers
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

            else if playerHasMoreThanOneCountry playingGameAttributes.players currentPlayerId then
                PlayerTurn TroopMovement currentPlayerId

            else
                PlayerTurn TroopPlacement (currentPlayerId |> nextPlayerCheckForDeadPlayers playingGameAttributes.players)
    in
    { playingGameAttributes
        | players = updatedPlayers
        , currentPlayerTurn = nextPlayerTurn
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


updatePlayerTroopCountForCountry : PlayerId -> GameMap.CountryId -> TroopCount.TroopCount -> Dict.Dict Int Player -> Dict.Dict Int Player
updatePlayerTroopCountForCountry playerId countryId troops players =
    -- Make this return result with error if dict lookup fails
    case getPlayer playerId players of
        Just player ->
            players
                |> updatePlayer
                    playerId
                    { player
                        | countryTroopCounts =
                            player.countryTroopCounts
                                |> updateTroopCount countryId troops
                    }

        Nothing ->
            -- This should never happen
            players


updatePlayersWithPlayer : PlayerId -> (Player -> Player) -> ActiveGame -> ActiveGame
updatePlayersWithPlayer playerId toUpdatedPlayer playingGameAttributes =
    case getPlayer playerId playingGameAttributes.players of
        Just player ->
            { playingGameAttributes
                | players =
                    playingGameAttributes.players
                        |> updatePlayer playerId (toUpdatedPlayer player)
            }

        Nothing ->
            { playingGameAttributes | error = Just "some error" }


updateTroopCount : GameMap.CountryId -> TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
updateTroopCount (GameMap.CountryId countryId) troopCount troopCounts =
    Dict.insert countryId troopCount troopCounts
