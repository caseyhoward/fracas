import * as Database from "../../Database";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as PubSub from "../../PubSub";
import * as Player from "../../models/Player";
import * as Models from "../../repositories/Models";
import * as graphql from "../../api/graphql";

export default async function startInternetGame(
  executeQuery: Database.ExecuteQuery,
  pubSub: PubSub.PubSub,
  input: graphql.RequireFields<
    graphql.MutationStartInternetGameArgs,
    "playerToken"
  >
): Promise<boolean> {
  const player = await InternetGamePlayerRepository.findByToken(executeQuery)(
    input.playerToken
  );

  const configuration = await InternetGameConfigurationRepository.findById(
    executeQuery
  )(player.gameId);
  if (Player.isCurrentUserHost(player.id, configuration.players)) {
    const game: Models.InternetGame = {
      __typename: "InternetGame",
      id: configuration.id,
      mapId: configuration.mapId,
      players: configuration.players.map(playerConfigurationToPlayer),
      neutralCountryTroops: generateRandomTroopCounts(),
      playerTurn: {
        __typename: "PlayerTurn",
        playerId: player.id,
        playerTurnStage: Models.PlayerTurnStage.CapitolPlacement
      }
    };

    await InternetGameRepository.save(executeQuery, game);
    PubSub.internetGameConfigurationChanged(pubSub);
    return true;
  } else {
    return false;
  }
}

function generateRandomTroopCounts(): Models.CountryTroopCounts[] {
  return [];
}

function playerConfigurationToPlayer(
  playerConfiguration: Player.PlayerConfiguration
): Models.Player {
  return {
    __typename: "Player",
    id: playerConfiguration.playerId,
    name: playerConfiguration.name,
    countryTroopCounts: [],
    color: playerConfiguration.color,
    ports: []
  };
}
