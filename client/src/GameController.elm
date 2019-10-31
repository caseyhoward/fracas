module GameController exposing (update)

import Country
import Dict
import Game
import Game.InfoPanel
import Map
import Player
import PlayerTurn
import TroopCount



-- This module is pretty much all the stuff that should be validated/executed on the server


update : Game.Msg -> Game.Model -> ( Game.Model, Cmd msg )
update msg model =
    case msg of
        Game.CountryMouseUp clickedCountryId ->
            ( handleCountryMouseUpFromPlayer clickedCountryId model.activeGame.currentUserPlayerId model, Cmd.none )

        Game.CountryMouseDown clickedCountryId ->
            ( handleCountryMouseDown clickedCountryId model, Cmd.none )

        Game.CountryMouseOut mouseOutCountryId ->
            ( handleCountryMouseOut mouseOutCountryId model, Cmd.none )

        Game.InfoPanelMsg (Game.InfoPanel.ShowAvailableMovesCheckboxToggled isChecked) ->
            ( { model | showAvailableMoves = isChecked }, Cmd.none )

        Game.MouseUp ->
            ( stopShowingCountryHelperOutlines model, Cmd.none )

        Game.InfoPanelMsg Game.InfoPanel.Pass ->
            ( updateModelWithGameResult (pass model.activeGame) model, Cmd.none )

        Game.InfoPanelMsg (Game.InfoPanel.TroopCountChanged numberOfTroopsToMoveString) ->
            ( { model | activeGame = updateNumberOfTroopsToMove numberOfTroopsToMoveString model.activeGame }, Cmd.none )

        Game.InfoPanelMsg Game.InfoPanel.CancelTroopMovement ->
            ( { model | activeGame = cancelMovingTroops model.activeGame }, Cmd.none )

        Game.ShowCountryBorderHelper ->
            ( makeCountryHelperOutlinesActive model, Cmd.none )


updateModelWithGameResult : Result Error Game.Game -> Game.Model -> Game.Model
updateModelWithGameResult result model =
    case result of
        Ok activeGame ->
            { model | activeGame = activeGame, error = Nothing }

        Err error ->
            { model | error = Just (errorToString error) }


handleCountryMouseUpFromPlayer : Country.Id -> Player.Id -> Game.Model -> Game.Model
handleCountryMouseUpFromPlayer clickedCountryId currentUserPlayerId model =
    case model.countryBorderHelperOutlineStatus of
        Game.CountryBorderHelperOutlineActive _ ->
            model

        Game.CountryBorderHelperOutlineInactive ->
            model

        Game.CountryBorderHelperOutlineWaitingForDelay countryToShowInfoForId ->
            if clickedCountryId == countryToShowInfoForId then
                case countryClicked clickedCountryId currentUserPlayerId model.activeGame of
                    Ok updatedGame ->
                        { model
                            | activeGame = updatedGame
                            , countryBorderHelperOutlineStatus = Game.CountryBorderHelperOutlineInactive
                        }

                    Err error ->
                        { model | error = Just (errorToString error) }

            else
                model


handleCountryMouseDown : Country.Id -> Game.Model -> Game.Model
handleCountryMouseDown countryId activeGame =
    { activeGame | countryBorderHelperOutlineStatus = Game.CountryBorderHelperOutlineWaitingForDelay countryId }


handleCountryMouseOut : Country.Id -> Game.Model -> Game.Model
handleCountryMouseOut mouseOutCountryId activeGame =
    case activeGame.countryBorderHelperOutlineStatus of
        Game.CountryBorderHelperOutlineWaitingForDelay countryId ->
            if countryId == mouseOutCountryId then
                { activeGame | countryBorderHelperOutlineStatus = Game.CountryBorderHelperOutlineInactive }

            else
                activeGame

        _ ->
            activeGame


stopShowingCountryHelperOutlines : Game.Model -> Game.Model
stopShowingCountryHelperOutlines activeGame =
    { activeGame | countryBorderHelperOutlineStatus = Game.CountryBorderHelperOutlineInactive }


makeCountryHelperOutlinesActive : Game.Model -> Game.Model
makeCountryHelperOutlinesActive model =
    case model.countryBorderHelperOutlineStatus of
        Game.CountryBorderHelperOutlineWaitingForDelay countryId ->
            { model | countryBorderHelperOutlineStatus = Game.CountryBorderHelperOutlineActive countryId }

        _ ->
            model


errorToString : Error -> String
errorToString (Error error) =
    error


countryClicked : Country.Id -> Player.Id -> Game.Game -> Result Error Game.Game
countryClicked clickedCountryId currentUserPlayerId activeGame =
    if PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn currentUserPlayerId then
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
                        attemptTroopMovement
                            { fromCountryId = fromCountryId
                            , clickedCountryId = clickedCountryId
                            , numberOfTroopsToMoveString = numberOfTroopsToMoveString
                            , map = activeGame |> Game.toMap
                            , activeGame = activeGame
                            }

                    PlayerTurn.GameOver ->
                        Ok activeGame

    else
        Ok activeGame



-- Exposed


cancelMovingTroops : Game.Game -> Game.Game
cancelMovingTroops activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn _ playerId ->
            { activeGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId }


pass : Game.Game -> Result Error Game.Game
pass activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected _ _) playerId ->
            Ok
                { activeGame
                    | currentPlayerTurn =
                        PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                }

        PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId ->
            Ok
                { activeGame
                    | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                }

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


updateNumberOfTroopsToMove : String -> Game.Game -> Game.Game
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


attackAnnexOrPort : Country.Id -> Player.Id -> Game.Game -> Result Error Game.Game
attackAnnexOrPort clickedCountryId currentPlayerId activeGame =
    case Game.getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        Game.OccupiedByCurrentPlayer _ ->
            attemptToBuildPort currentPlayerId clickedCountryId activeGame

        Game.OccupiedByOpponent opponentPlayerId ->
            attemptToAttackCountry opponentPlayerId clickedCountryId activeGame

        Game.Unoccupied ->
            attemptToAnnexCountry currentPlayerId clickedCountryId activeGame


attemptTroopMovement :
    { fromCountryId : Country.Id
    , clickedCountryId : Country.Id
    , numberOfTroopsToMoveString : String
    , activeGame : Game.Game
    , map : Map.Map
    }
    -> Result Error Game.Game
attemptTroopMovement { fromCountryId, clickedCountryId, numberOfTroopsToMoveString, activeGame, map } =
    case Game.getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        Game.OccupiedByCurrentPlayer playerCountryToTroopCount ->
            case String.toInt numberOfTroopsToMoveString of
                Just numberOfTroopsToMove ->
                    if Game.isCountryReachableFromOtherCountry fromCountryId clickedCountryId map.countries activeGame.players then
                        let
                            fromCountryTroopCount =
                                case Player.getPlayer (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) activeGame.players of
                                    Just currentPlayer1 ->
                                        case Game.getTroopCount fromCountryId currentPlayer1.countryTroopCounts of
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
                                        | currentPlayerTurn =
                                            PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement
                                                (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn
                                                    |> nextPlayerCheckForDeadPlayers activeGame.players
                                                )
                                    }
                                )

                    else
                        "You can't move troops between those countries" |> Error |> Err

                Nothing ->
                    "Number of troops must be a number" |> Error |> Err

        _ ->
            "You must move troops to your own country" |> Error |> Err


attemptToPlaceCapitol : Country.Id -> Player.Id -> Game.Game -> Result Error Game.Game
attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame =
    case Game.getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        Game.OccupiedByCurrentPlayer _ ->
            "Error: Somehow you are placing a second capitol" |> Error |> Err

        Game.OccupiedByOpponent _ ->
            "You must select an unoccuppied country" |> Error |> Err

        Game.Unoccupied ->
            case Player.getPlayer currentPlayerId activeGame.players of
                Just currentPlayer ->
                    let
                        neutralTroopCount =
                            Game.getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

                        updatedPlayer =
                            { currentPlayer
                                | countryTroopCounts =
                                    updateTroopCount clickedCountryId neutralTroopCount currentPlayer.countryTroopCounts
                                , capitolStatus = Player.Capitol clickedCountryId
                            }

                        updatedPlayers =
                            updatePlayer currentPlayerId updatedPlayer activeGame.players

                        nextPlayerTurn =
                            case currentPlayerId of
                                Player.Id id ->
                                    if Just id == (Dict.keys updatedPlayers |> List.reverse |> List.head) then
                                        PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (nextPlayerId updatedPlayers currentPlayerId)

                                    else
                                        PlayerTurn.PlayerTurn PlayerTurn.CapitolPlacement (nextPlayerId updatedPlayers currentPlayerId)
                    in
                    Ok
                        { activeGame
                            | players = updatedPlayers
                            , neutralCountryTroops = destroyTroops clickedCountryId activeGame.neutralCountryTroops
                            , currentPlayerTurn = nextPlayerTurn
                        }

                Nothing ->
                    "Something bad happened" |> Error |> Err


nextPlayerId : Player.Players -> Player.Id -> Player.Id
nextPlayerId players (Player.Id currentTurnPlayerId) =
    let
        numberOfPlayers =
            Dict.size players

        playersByIndex : Dict.Dict Int String
        playersByIndex =
            players
                |> Dict.toList
                |> List.indexedMap (\index ( playerId, _ ) -> ( index, playerId ))
                |> Dict.fromList

        currentPlayerIndex : Int
        currentPlayerIndex =
            playersByIndex
                |> Dict.foldl
                    (\index playerId result ->
                        if playerId == currentTurnPlayerId then
                            index

                        else
                            result
                    )
                    -1

        nextPlayerIndex : Int
        nextPlayerIndex =
            remainderBy numberOfPlayers (currentPlayerIndex + 1)

        next =
            playersByIndex
                |> Dict.get nextPlayerIndex
                |> Maybe.withDefault "-1"
    in
    Player.Id next


attemptTroopPlacement : Country.Id -> Player.Id -> TroopCount.TroopCount -> Game.Game -> Result Error Game.Game
attemptTroopPlacement clickedCountryId currentPlayerId troopsToPlace activeGame =
    case Game.getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        Game.OccupiedByCurrentPlayer clickedCountryTroopCount ->
            let
                updatedGameResult =
                    activeGame |> updatePlayerTroopCountForCountry currentPlayerId clickedCountryId (TroopCount.addTroopCounts clickedCountryTroopCount troopsToPlace)
            in
            updatedGameResult
                |> Result.map
                    (\updatedGame ->
                        { updatedGame
                            | currentPlayerTurn =
                                PlayerTurn.PlayerTurn PlayerTurn.AttackAnnexOrPort currentPlayerId
                        }
                    )

        Game.OccupiedByOpponent _ ->
            "You must put troops in your own country" |> Error |> Err

        Game.Unoccupied ->
            "You must put troops in your own country" |> Error |> Err


attemptToBuildPort : Player.Id -> Country.Id -> Game.Game -> Result Error Game.Game
attemptToBuildPort currentPlayerId clickedCountryId activeGame =
    case Game.getTroopCountForCountry clickedCountryId activeGame.players of
        Just _ ->
            buildPort currentPlayerId clickedCountryId activeGame

        Nothing ->
            "You can't build a port in a country you don't own" |> Error |> Err


attemptSelectTroopMovementFromCountry : Country.Id -> Player.Id -> Game.Game -> Result Error Game.Game
attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame =
    case Game.getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        Game.OccupiedByCurrentPlayer troopCount ->
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


attemptToAnnexCountry : Player.Id -> Country.Id -> Game.Game -> Result Error Game.Game
attemptToAnnexCountry currentPlayerId clickedCountryId activeGame =
    if Game.canAnnexCountry (activeGame |> Game.toMap) currentPlayerId activeGame.players clickedCountryId then
        let
            neutralTroopCount =
                Game.getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

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


attemptToAttackCountry : Player.Id -> Country.Id -> Game.Game -> Result Error Game.Game
attemptToAttackCountry opponentPlayerId clickedCountryId activeGame =
    case Game.attackResult clickedCountryId (activeGame |> Game.toMap) activeGame.players activeGame.currentPlayerTurn of
        Game.OpponentCountryLosesTroops remainingTroops ->
            activeGame
                |> updatePlayerTroopCountForCountry opponentPlayerId clickedCountryId remainingTroops
                |> Result.map updateForSuccessfulAttack

        Game.OpponentEliminated ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> Result.andThen (destroyPlayer opponentPlayerId)
                |> Result.map updateForSuccessfulAttack

        Game.CurrentPlayerAcquiresOpponentCountry ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> Result.map updateForSuccessfulAttack

        Game.NotEnoughTroopsToAttack attackStrength defenseStrength ->
            ("Not enough to attack: attack strength = " ++ TroopCount.toString attackStrength ++ ", defense strength = " ++ TroopCount.toString defenseStrength)
                |> Error
                |> Err

        Game.AttackResultError errorMessage ->
            errorMessage |> Error |> Err


buildPort : Player.Id -> Country.Id -> Game.Game -> Result Error Game.Game
buildPort playerId countryId activeGame =
    -- We already had to check that the player owned this country before so no need to do that here
    case Map.isCountryNeighboringWater countryId (activeGame |> Game.toMap |> .countries) of
        Just isNeighboringWater ->
            if isNeighboringWater then
                case Game.getCountryHasPort countryId activeGame.players of
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
                                |> Result.map
                                    (\updated ->
                                        { updated
                                            | currentPlayerTurn = nextPlayerTurn
                                        }
                                    )

                    Nothing ->
                        "Error while building port" |> Error |> Err

            else
                "A country must be next to water to build a port" |> Error |> Err

        Nothing ->
            "Error checking if country borders water" |> Error |> Err


destroyPlayer : Player.Id -> Game.Game -> Result Error Game.Game
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



-- TODO


nextPlayerCheckForDeadPlayers : Player.Players -> Player.Id -> Player.Id
nextPlayerCheckForDeadPlayers players currentPlayerId =
    -- This doesn't work during capitol placement because nobody will have a capitol except player 1 after player 1 places their capitol
    let
        playerId =
            currentPlayerId |> nextPlayerId players
    in
    case Player.getPlayer playerId players of
        Just newCurrentPlayer ->
            case newCurrentPlayer |> .capitolStatus of
                Player.Capitol _ ->
                    playerId

                Player.NoCapitol ->
                    playerId |> nextPlayerCheckForDeadPlayers players

        Nothing ->
            currentPlayerId


playerHasMoreThanOneCountry : Player.Players -> Player.Id -> Bool
playerHasMoreThanOneCountry players playerId =
    Player.getPlayer playerId players
        |> Maybe.map (\player -> Dict.size player.countryTroopCounts > 1)
        |> Maybe.withDefault False


playerTurnToPlayerId : PlayerTurn.PlayerTurn -> Player.Id
playerTurnToPlayerId (PlayerTurn.PlayerTurn _ playerId) =
    playerId


removePlayerCountry : Country.Id -> Game.Game -> Result Error Game.Game
removePlayerCountry (Country.Id countryId) activeGame =
    -- Make this return result with error if dict lookup fails
    case Game.findCountryOwner (Country.Id countryId) activeGame.players of
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


takeCountryFromOpponent : Country.Id -> Game.Game -> Result Error Game.Game
takeCountryFromOpponent countryId activeGame =
    activeGame
        |> removePlayerCountry countryId
        |> Result.andThen
            (\updatedGame ->
                updatedGame |> updatePlayerTroopCountForCountry (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) countryId TroopCount.noTroops
            )


updateForSuccessfulAttack : Game.Game -> Game.Game
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


updatePlayerTroopCountForCountry : Player.Id -> Country.Id -> TroopCount.TroopCount -> Game.Game -> Result Error Game.Game
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


updatePlayersWithPlayer : Player.Id -> (Player.Player -> Player.Player) -> Game.Game -> Result Error Game.Game
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


type Error
    = Error String
