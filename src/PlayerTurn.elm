module PlayerTurn exposing
    ( PlayerTurn(..)
    , PlayerTurnStage(..)
    , canCurrentPlayerPass
    , getCurrentPlayer
    , getPlayerTurnStageFromPlayerTurn
    , isCapitolPlacementTurn
    , isPlayerTurn
    , playerTurnInput
    , selectionSet
    , toString
    , troopsToMove
    )

import Api.Enum.PlayerTurnStage
import Api.InputObject
import Api.Object
import Api.Object.PlayerTurn
import Country
import Graphql.SelectionSet exposing (SelectionSet)
import Player
import TroopCount


type PlayerTurn
    = PlayerTurn PlayerTurnStage Player.Id


type PlayerTurnStage
    = CapitolPlacement
    | TroopPlacement
    | AttackAnnexOrPort
    | TroopMovement
    | TroopMovementFromSelected Country.Id String
    | GameOver


playerTurnInput : PlayerTurn -> Api.InputObject.PlayerTurnInput
playerTurnInput (PlayerTurn playerTurnStage (Player.Id playerId)) =
    { playerId = playerId |> String.fromInt
    , playerTurnStage = playerTurnStage |> playerTurnStageInput
    }


getPlayerTurnStageFromPlayerTurn : PlayerTurn -> PlayerTurnStage
getPlayerTurnStageFromPlayerTurn playerTurn =
    case playerTurn of
        PlayerTurn playerTurnStage _ ->
            playerTurnStage


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


getCurrentPlayer : PlayerTurn -> Player.Id
getCurrentPlayer currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn _ playerId ->
            playerId


isCapitolPlacementTurn : PlayerTurn -> Bool
isCapitolPlacementTurn currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn CapitolPlacement _ ->
            True

        _ ->
            False


isPlayerTurn : PlayerTurn -> Player.Id -> Bool
isPlayerTurn (PlayerTurn _ playerTurnPlayerId) playerId =
    playerId == playerTurnPlayerId


troopsToMove : PlayerTurn -> Maybe String
troopsToMove currentPlayerTurn =
    case currentPlayerTurn of
        PlayerTurn (TroopMovementFromSelected _ troops) _ ->
            Just troops

        _ ->
            Nothing


selectionSet : SelectionSet PlayerTurn Api.Object.PlayerTurn
selectionSet =
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
                (playerId |> String.toInt |> Maybe.withDefault 0 |> Player.Id)
        )
        Api.Object.PlayerTurn.playerId
        Api.Object.PlayerTurn.playerTurnStage


playerTurnStageInput : PlayerTurnStage -> Api.Enum.PlayerTurnStage.PlayerTurnStage
playerTurnStageInput playerTurnStage =
    case playerTurnStage of
        CapitolPlacement ->
            Api.Enum.PlayerTurnStage.CapitolPlacement

        TroopPlacement ->
            Api.Enum.PlayerTurnStage.TroopPlacement

        AttackAnnexOrPort ->
            Api.Enum.PlayerTurnStage.AttackAnnexOrPort

        TroopMovement ->
            Api.Enum.PlayerTurnStage.TroopMovement

        TroopMovementFromSelected _ _ ->
            Debug.todo ""

        -- Api.Enum.PlayerTurnStage.TroopMovement
        GameOver ->
            Api.Enum.PlayerTurnStage.GameOver


toString : Player.Players -> PlayerTurn -> String
toString players (PlayerTurn playerTurnStage playerId) =
    case Player.getPlayerName playerId players of
        Just playerName ->
            case playerTurnStage of
                CapitolPlacement ->
                    playerName ++ ": Choose your first country. This country will be your capitol. If it is captured, you lose."

                TroopPlacement ->
                    playerName ++ ": Place " ++ (Player.numberOfTroopsToPlace playerId players |> TroopCount.pluralize) ++ " in one of your countries"

                AttackAnnexOrPort ->
                    playerName ++ ": Choose an enemy country to attack, a neutral country to annex, or one of your countries bordering water to build a port"

                TroopMovement ->
                    playerName ++ ": Choose a country to move troops from or press the \"Pass\" button for no troop movement"

                TroopMovementFromSelected _ _ ->
                    playerName ++ ": Enter the number of troops to move and choose a destination or press the \"Pass\" button for no movement"

                GameOver ->
                    playerName ++ " has won the game!!!"

        Nothing ->
            ""
