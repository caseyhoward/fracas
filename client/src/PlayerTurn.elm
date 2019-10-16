module PlayerTurn exposing
    ( PlayerTurn(..)
    , PlayerTurnStage(..)
    , canCurrentPlayerPass
    , firstTurn
    , getCurrentPlayer
    , getPlayerTurnStageFromPlayerTurn
    , input
    , isCapitolPlacementTurn
    , isPlayerTurn
    , selectionSet
    , toString
    , troopsToMove
    )

import Api.Enum.PlayerTurnStage
import Api.InputObject
import Api.Object
import Api.Object.PlayerTurn
import Country
import Graphql.OptionalArgument
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


input : PlayerTurn -> Api.InputObject.PlayerTurnInput
input (PlayerTurn playerTurnStage (Player.Id playerId)) =
    Api.InputObject.buildPlayerTurnInput
        { playerId = playerId
        , playerTurnStage = playerTurnStage |> playerTurnStageInput
        }
        (\optional ->
            case playerTurnStage of
                TroopMovementFromSelected countryId troopCount ->
                    { optional
                        | fromCountryId = Graphql.OptionalArgument.Present (Country.idToString countryId)
                        , troopCount = Graphql.OptionalArgument.Present troopCount
                    }

                _ ->
                    optional
        )


firstTurn : PlayerTurn
firstTurn =
    PlayerTurn CapitolPlacement (Player.Id "0")


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
    Graphql.SelectionSet.map4
        (\playerId playerTurnStage maybeFromCountryId maybeTroopCount ->
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

                    Api.Enum.PlayerTurnStage.TroopMovementFromSelected ->
                        case ( maybeFromCountryId, maybeTroopCount ) of
                            ( Just fromCountryId, Just troopCount ) ->
                                TroopMovementFromSelected (fromCountryId |> Country.Id) troopCount

                            _ ->
                                TroopMovementFromSelected ("-1" |> Country.Id) ""

                    Api.Enum.PlayerTurnStage.GameOver ->
                        GameOver
                )
                (playerId |> Player.Id)
        )
        Api.Object.PlayerTurn.playerId
        Api.Object.PlayerTurn.playerTurnStage
        Api.Object.PlayerTurn.fromCountryId
        Api.Object.PlayerTurn.troopCount


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
            Api.Enum.PlayerTurnStage.TroopMovementFromSelected

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
