module ActiveGame exposing
    ( ActiveGame
    , CapitolStatus(..)
    , CountryBorderHelperOutlineStatus(..)
    , CountryToRender
    , Error
    , Player
    , PlayerId(..)
    , PlayerTurn(..)
    , PlayerTurnStage(..)
    , canCurrentPlayerCancelTroopMovement
    , canCurrentPlayerPass
    , cancelMovingTroops
    , errorToString
    , findCountryOwner
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
    , getSelectedCountryForTroopMovement
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
    , playerIdToString
    , playerTurnToString
    , start
    , stopShowingCountryHelperOutlines
    , troopsToMove
    , updateNumberOfTroopsToMove
    , waitingToShowCountryHelperOutlines
    )

import Color
import Dict
import GameMap
import Set
import TroopCount
import ViewHelpers


type alias ActiveGame =
    { currentPlayerTurn : PlayerTurn
    , map : GameMap.GameMap
    , players : Players
    , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
    , numberOfPlayers : Int
    , countryBorderHelperOutlines : CountryBorderHelperOutlineStatus
    , showAvailableMoves : Bool
    }


type alias CountryToRender =
    { id : GameMap.CountryId
    , color : Color.Color
    , troopCount : TroopCount.TroopCount
    , center : ( Float, Float )
    , polygonPoints : List ( Float, Float )
    , capitolDots : Maybe (Set.Set ( Float, Float ))
    , canBeClicked : Bool
    , isBeingMovedFrom : Bool
    , portSegments : Maybe (Set.Set ( ( Float, Float ), ( Float, Float ) ))
    }


type alias Players =
    Dict.Dict Int Player


type alias Countries =
    Dict.Dict String GameMap.Country


type Error
    = Error String


errorToString : Error -> String
errorToString (Error error) =
    error


getAttackStrengthPerPlayer : GameMap.GameMap -> Players -> GameMap.CountryId -> Dict.Dict Int TroopCount.TroopCount
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


getCountryAttackers : GameMap.GameMap -> Players -> GameMap.CountryId -> CountryAttackers
getCountryAttackers gameMap players countryId =
    let
        neighborCountriesByPlayer : List ( PlayerId, GameMap.CountryId )
        neighborCountriesByPlayer =
            case GameMap.getCountry countryId gameMap.countries of
                Just country ->
                    country.neighboringCountries
                        |> Set.foldl
                            (\neighborCountry result ->
                                case findCountryOwner (GameMap.CountryId neighborCountry) players of
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
                        , getTroopCountForCountry neighborCountryId players |> Maybe.withDefault TroopCount.noTroops
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
            GameMap.getCountriesThatCanReachCountryThroughWater gameMap countryId

        attackerCountriesNeighoboringWater : List GameMap.CountryId
        attackerCountriesNeighoboringWater =
            countriesReachableThroughWater

        attackerCountriesNeighoboringWaterWithPort : List GameMap.CountryId
        attackerCountriesNeighoboringWaterWithPort =
            attackerCountriesNeighoboringWater
                |> filterCountriesWithPort players

        waterNeighborCountriesByPlayer : List ( PlayerId, GameMap.CountryId )
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

        waterNeighborCountriesByPlayerTroopCounts : List ( PlayerId, GameMap.CountryId, TroopCount.TroopCount )
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


getCountriesToRender : ActiveGame -> Maybe (List CountryToRender)
getCountriesToRender activeGame =
    activeGame.map.countries
        |> Dict.map
            (\countryId country ->
                let
                    countryOwnerAndTroopCount =
                        findCountryOwnerAndTroopCount (GameMap.CountryId countryId) activeGame.players

                    shift =
                        \_ ->
                            case country.center of
                                ( medianX, medianY ) ->
                                    ( (toFloat medianX + 0.5) * toFloat ViewHelpers.pixelsPerMapSquare, (toFloat medianY + 0.5) * toFloat ViewHelpers.pixelsPerMapSquare )
                in
                case countryOwnerAndTroopCount of
                    Just ( countryOwnerId, troopCount ) ->
                        getPlayer countryOwnerId activeGame.players
                            |> Maybe.map
                                (\countryOwner ->
                                    { id = GameMap.CountryId countryId
                                    , troopCount = troopCount
                                    , center = country.center |> shift
                                    , polygonPoints = country.polygon
                                    , color = countryOwner.color
                                    , capitolDots =
                                        case countryOwner.capitolStatus of
                                            Capitol (GameMap.CountryId capitolId) coordinates ->
                                                if capitolId == countryId then
                                                    Just coordinates

                                                else
                                                    Nothing

                                            NoCapitol ->
                                                Nothing
                                    , canBeClicked = getCountryCanBeClicked activeGame (GameMap.CountryId countryId)
                                    , isBeingMovedFrom = getIsBeingMovedFrom activeGame (GameMap.CountryId countryId)
                                    , portSegments = getPortSegments (GameMap.CountryId countryId) country activeGame.players
                                    }
                                )

                    Nothing ->
                        Just
                            { id = GameMap.CountryId countryId
                            , troopCount = getTroopCount (GameMap.CountryId countryId) activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops
                            , center = country.center |> shift
                            , color = neutralCountryColor
                            , polygonPoints = country.polygon
                            , capitolDots = Nothing
                            , canBeClicked = getCountryCanBeClicked activeGame (GameMap.CountryId countryId)
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


type CountryBorderHelperOutlineStatus
    = CountryBorderHelperOutlineWaitingForDelay GameMap.CountryId
    | CountryBorderHelperOutlineInactive
    | CountryBorderHelperOutlineActive GameMap.CountryId


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


canCurrentPlayerCancelTroopMovement : ActiveGame -> Bool
canCurrentPlayerCancelTroopMovement activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn playerTurnStage _ ->
            case playerTurnStage of
                TroopMovementFromSelected _ _ ->
                    True

                _ ->
                    False


getCurrentPlayer : PlayerTurn -> PlayerId
getCurrentPlayer currentPlayerTurn =
    case currentPlayerTurn of
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
            getCountryAttackers activeGame.map activeGame.players countryToDefend

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


getSelectedCountryForTroopMovement : ActiveGame -> Maybe GameMap.CountryId
getSelectedCountryForTroopMovement activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected selectedCountryId _) _ ->
            Just selectedCountryId

        _ ->
            Nothing


findCountryOwner : GameMap.CountryId -> Players -> Maybe PlayerId
findCountryOwner countryId players =
    findCountryOwnerAndTroopCount countryId players
        |> Maybe.map Tuple.first


getCountryHasPort : GameMap.CountryId -> Players -> Maybe Bool
getCountryHasPort (GameMap.CountryId countryId) players =
    findCountryOwner (GameMap.CountryId countryId) players
        |> Maybe.andThen
            (\playerId ->
                getPlayer playerId players
                    |> Maybe.map .ports
                    |> Maybe.map
                        (Set.member countryId)
            )


getDefaultColor : PlayerId -> Color.Color
getDefaultColor (PlayerId playerId) =
    case Dict.get playerId defaultPlayerColors of
        Just color ->
            color

        Nothing ->
            Color.black


getPlayerCountryAndTroopCounts :
    { players : Players, currentPlayerTurn : PlayerTurn }
    -> List { playerId : PlayerId, countryCount : Int, troopCount : TroopCount.TroopCount, isAlive : Bool }
getPlayerCountryAndTroopCounts { players, currentPlayerTurn } =
    players
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


getTroopCount : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Maybe TroopCount.TroopCount
getTroopCount (GameMap.CountryId countryId) troopCounts =
    Dict.get countryId troopCounts


getTroopCountForCountry : GameMap.CountryId -> Players -> Maybe TroopCount.TroopCount
getTroopCountForCountry countryId players =
    findCountryOwner countryId players
        |> Maybe.andThen (\playerId -> getPlayer playerId players)
        |> Maybe.andThen (\player -> getTroopCount countryId player.countryTroopCounts)


getPlayerColorFromPlayerTurn : Players -> PlayerTurn -> Color.Color
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


handleCountryMouseUpFromPlayer : GameMap.CountryId -> ActiveGame -> Result Error ActiveGame
handleCountryMouseUpFromPlayer clickedCountryId activeGame =
    let
        updatedActiveGameResult =
            case activeGame.countryBorderHelperOutlines of
                CountryBorderHelperOutlineActive _ ->
                    Ok activeGame

                CountryBorderHelperOutlineInactive ->
                    Ok activeGame

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
                                        Ok activeGame

                    else
                        Ok activeGame
    in
    updatedActiveGameResult
        |> Result.map
            (\updatedActiveGame ->
                { updatedActiveGame | countryBorderHelperOutlines = CountryBorderHelperOutlineInactive }
            )


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


isCapitolPlacementTurn : PlayerTurn -> Bool
isCapitolPlacementTurn currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn CapitolPlacement _ ->
            True

        _ ->
            False


isCountryIdCapitol : PlayerId -> GameMap.CountryId -> Players -> Maybe Bool
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


makeCountryHelperOutlinesActive : ActiveGame -> ActiveGame
makeCountryHelperOutlinesActive activeGame =
    case activeGame.countryBorderHelperOutlines of
        CountryBorderHelperOutlineWaitingForDelay countryId ->
            { activeGame | countryBorderHelperOutlines = CountryBorderHelperOutlineActive countryId }

        _ ->
            activeGame


numberOfTroopsToPlace : PlayerId -> Players -> TroopCount.TroopCount
numberOfTroopsToPlace playerId players =
    case getPlayer playerId players of
        Just player ->
            TroopCount.numberOfTroopsToPlace (Dict.size player.countryTroopCounts) troopsPerCountryPerTurn

        Nothing ->
            -- TODO : Propogate error
            TroopCount.noTroops


pass : ActiveGame -> Result Error ActiveGame
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
    , numberOfPlayers = numberOfPlayers
    , neutralCountryTroops = neutralTroopCounts
    , showAvailableMoves = False
    , countryBorderHelperOutlines = CountryBorderHelperOutlineInactive
    }


troopsToMove : PlayerTurn -> Maybe String
troopsToMove currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected _ troops) _ ->
            Just troops

        _ ->
            Nothing


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



------- LOCAL


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


addPortForPlayer : GameMap.CountryId -> Player -> Player
addPortForPlayer (GameMap.CountryId countryId) player =
    { player | ports = player.ports |> Set.insert countryId }


attackAnnexOrPort : GameMap.CountryId -> PlayerId -> ActiveGame -> Result Error ActiveGame
attackAnnexOrPort clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame of
        OccupiedByCurrentPlayer _ ->
            attemptToBuildPort currentPlayerId clickedCountryId activeGame

        OccupiedByOpponent opponentPlayerId ->
            attemptToAttackCountry opponentPlayerId clickedCountryId activeGame

        Unoccupied ->
            attemptToAnnexCountry currentPlayerId clickedCountryId activeGame


attemptTroopMovement : GameMap.CountryId -> GameMap.CountryId -> String -> ActiveGame -> Result Error ActiveGame
attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame =
    case getCountryStatus clickedCountryId activeGame of
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


attemptToPlaceCapitol : GameMap.CountryId -> PlayerId -> ActiveGame -> Result Error ActiveGame
attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame =
    case GameMap.getCountry clickedCountryId activeGame.map.countries of
        Just clickedCountry ->
            case getCountryStatus clickedCountryId activeGame of
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
                                        , capitolStatus = Capitol clickedCountryId (GameMap.capitolDotsCoordinates clickedCountry.coordinates ViewHelpers.pixelsPerMapSquare)
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


attemptTroopPlacement : GameMap.CountryId -> PlayerId -> TroopCount.TroopCount -> ActiveGame -> Result Error ActiveGame
attemptTroopPlacement clickedCountryId currentPlayerId troopsToPlace activeGame =
    case getCountryStatus clickedCountryId activeGame of
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


attemptToBuildPort : PlayerId -> GameMap.CountryId -> ActiveGame -> Result Error ActiveGame
attemptToBuildPort currentPlayerId clickedCountryId activeGame =
    case getTroopCountForCountry clickedCountryId activeGame.players of
        Just _ ->
            buildPort currentPlayerId clickedCountryId activeGame

        Nothing ->
            "You can't build a port in a country you don't own" |> Error |> Err


attackResult : GameMap.CountryId -> ActiveGame -> AttackResult
attackResult clickedCountryId activeGame =
    case findCountryOwner clickedCountryId activeGame.players of
        Just opponentPlayerId ->
            let
                countryAttackers =
                    getAttackStrengthPerPlayer activeGame.map activeGame.players clickedCountryId

                currentPlayerId =
                    getCurrentPlayer activeGame.currentPlayerTurn

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


attemptSelectTroopMovementFromCountry : GameMap.CountryId -> PlayerId -> ActiveGame -> Result Error ActiveGame
attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame of
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


attemptToAnnexCountry : PlayerId -> GameMap.CountryId -> ActiveGame -> Result Error ActiveGame
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


attemptToAttackCountry : PlayerId -> GameMap.CountryId -> ActiveGame -> Result Error ActiveGame
attemptToAttackCountry opponentPlayerId clickedCountryId activeGame =
    case attackResult clickedCountryId activeGame of
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


filterCountriesOwnedBy : Players -> PlayerId -> List GameMap.CountryId -> List GameMap.CountryId
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


buildPort : PlayerId -> GameMap.CountryId -> ActiveGame -> Result Error ActiveGame
buildPort playerId countryId activeGame =
    -- We already had to check that the player owned this country before so no need to do that here
    case GameMap.isCountryNeighboringWater countryId activeGame.map.countries of
        Just isNeighboringWater ->
            if isNeighboringWater then
                case getCountryHasPort countryId activeGame.players of
                    Just hasPort ->
                        if hasPort then
                            "This country already has a port" |> Error |> Err

                        else
                            let
                                updatedActiveGameResult =
                                    activeGame
                                        |> updatePlayersWithPlayer playerId (addPortForPlayer countryId)

                                nextPlayerTurn =
                                    if playerHasMoreThanOneCountry activeGame.players playerId then
                                        PlayerTurn TroopMovement playerId

                                    else
                                        PlayerTurn TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                            in
                            updatedActiveGameResult
                                |> Result.map (\updated -> { updated | currentPlayerTurn = nextPlayerTurn })

                    Nothing ->
                        "Error while building port" |> Error |> Err

            else
                "A country must be next to water to build a port" |> Error |> Err

        Nothing ->
            "Error checking if country borders water" |> Error |> Err


canAnnexCountry : GameMap.GameMap -> PlayerId -> Players -> GameMap.CountryId -> Bool
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


destroyPlayer : PlayerId -> ActiveGame -> Result Error ActiveGame
destroyPlayer playerId activeGame =
    -- Make this return result with error if dict lookup fails
    activeGame
        |> updatePlayersWithPlayer
            playerId
            (\player ->
                { player | capitolStatus = NoCapitol, countryTroopCounts = Dict.empty }
            )


destroyTroops : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
destroyTroops (GameMap.CountryId countryId) neutralTroopCounts =
    Dict.remove countryId neutralTroopCounts


filterCountriesWithPort : Players -> List GameMap.CountryId -> List GameMap.CountryId
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


findCountryOwnerAndTroopCount : GameMap.CountryId -> Players -> Maybe ( PlayerId, TroopCount.TroopCount )
findCountryOwnerAndTroopCount (GameMap.CountryId countryId) players =
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


getCountryCanBeClicked : ActiveGame -> GameMap.CountryId -> Bool
getCountryCanBeClicked activeGame countryId =
    case activeGame.currentPlayerTurn of
        PlayerTurn playerTurnStatus currentPlayerId ->
            case playerTurnStatus of
                CapitolPlacement ->
                    case findCountryOwner countryId activeGame.players of
                        Just _ ->
                            False

                        Nothing ->
                            True

                TroopPlacement ->
                    case findCountryOwner countryId activeGame.players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                AttackAnnexOrPort ->
                    let
                        canAttack =
                            case attackResult countryId activeGame of
                                NotEnoughTroopsToAttack _ _ ->
                                    False

                                AttackResultError _ ->
                                    False

                                _ ->
                                    True

                        canAnnex =
                            canAnnexCountry activeGame.map currentPlayerId activeGame.players countryId

                        canBuildPort =
                            False
                    in
                    canAttack || canAnnex || canBuildPort

                TroopMovement ->
                    case findCountryOwner countryId activeGame.players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                TroopMovementFromSelected fromCountryId _ ->
                    if isCountryReachableFromOtherCountry fromCountryId countryId (getCurrentPlayer activeGame.currentPlayerTurn) activeGame.map.countries activeGame.players then
                        case getPlayer (getCurrentPlayer activeGame.currentPlayerTurn) activeGame.players of
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


getCountryStatus : GameMap.CountryId -> ActiveGame -> CountryStatus
getCountryStatus countryId activeGame =
    case getPlayer (getCurrentPlayer activeGame.currentPlayerTurn) activeGame.players of
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


getIsBeingMovedFrom : ActiveGame -> GameMap.CountryId -> Bool
getIsBeingMovedFrom activeGame countryId =
    case activeGame.currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected fromCountryId _) _ ->
            fromCountryId == countryId

        _ ->
            False


getPlayerName : PlayerId -> Players -> Maybe String
getPlayerName playerId players =
    getPlayer playerId players
        |> Maybe.map .name


getPortSegments : GameMap.CountryId -> GameMap.Country -> Players -> Maybe (Set.Set ( ( Float, Float ), ( Float, Float ) ))
getPortSegments countryId country players =
    getCountryHasPort countryId players
        |> Maybe.andThen
            (\hasPort ->
                if hasPort then
                    Just country.waterEdges

                else
                    Nothing
            )


isCountryOwnedByPlayer : PlayerId -> GameMap.CountryId -> Players -> Bool
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


isCountryReachableFromOtherCountry : GameMap.CountryId -> GameMap.CountryId -> PlayerId -> Dict.Dict String GameMap.Country -> Players -> Bool
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
                Capitol _ _ ->
                    nextPlayerId

                NoCapitol ->
                    nextPlayerId |> nextPlayerCheckForDeadPlayers players

        Nothing ->
            currentPlayerId


neutralCountryColor : Color.Color
neutralCountryColor =
    Color.gray


playerHasMoreThanOneCountry : Players -> PlayerId -> Bool
playerHasMoreThanOneCountry players playerId =
    getPlayer playerId players
        |> Maybe.map (\player -> Dict.size player.countryTroopCounts > 1)
        |> Maybe.withDefault False


playerTurnToPlayerId : PlayerTurn -> PlayerId
playerTurnToPlayerId (PlayerTurn _ playerId) =
    playerId


removePlayerCountry : GameMap.CountryId -> ActiveGame -> Result Error ActiveGame
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
            "Error finding country owner" |> Error |> Err


removeTroopCount : GameMap.CountryId -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
removeTroopCount (GameMap.CountryId countryId) troopCounts =
    Dict.remove countryId troopCounts


takeCountryFromOpponent : GameMap.CountryId -> ActiveGame -> Result Error ActiveGame
takeCountryFromOpponent countryId activeGame =
    activeGame
        |> removePlayerCountry countryId
        |> Result.andThen
            (\updatedActiveGame ->
                updatedActiveGame |> updatePlayerTroopCountForCountry (getCurrentPlayer activeGame.currentPlayerTurn) countryId TroopCount.noTroops
            )


troopsPerCountryPerTurn : Int
troopsPerCountryPerTurn =
    1


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
    }


updatePlayer : PlayerId -> Player -> Players -> Players
updatePlayer (PlayerId playerId) player players =
    Dict.insert playerId player players


updatePlayerTroopCountForCountry : PlayerId -> GameMap.CountryId -> TroopCount.TroopCount -> ActiveGame -> Result Error ActiveGame
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


updatePlayersWithPlayer : PlayerId -> (Player -> Player) -> ActiveGame -> Result Error ActiveGame
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


updateTroopCount : GameMap.CountryId -> TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
updateTroopCount (GameMap.CountryId countryId) troopCount troopCounts =
    Dict.insert countryId troopCount troopCounts
