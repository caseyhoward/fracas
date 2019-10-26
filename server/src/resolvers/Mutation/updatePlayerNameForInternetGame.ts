import * as Database from "../../Database";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as PubSub from "../../PubSub";
import * as Models from "../../repositories/Models";
import * as Graphql from "../../api/graphql";

export default async function updatePlayerNameForInternetGame(
  executeQuery: Database.ExecuteQuery,
  pubSub: PubSub.PubSub,
  input: Graphql.RequireFields<
    Graphql.MutationUpdatePlayerNameForInternetGameArgs,
    "name" | "playerToken"
  >
): Promise<boolean> {
  const internetGamePlayer = await InternetGamePlayerRepository.findByToken(
    executeQuery
  )(input.playerToken);
  const configuration = await InternetGameConfigurationRepository.findById(
    executeQuery
  )(internetGamePlayer.gameId);
  const updatedConfiguration = {
    ...configuration,
    players: configuration.players.map(player => {
      if (player.playerId == internetGamePlayer.id) {
        return { ...player, name: input.name };
      } else {
        return player;
      }
    })
  };
  await InternetGameConfigurationRepository.save(
    executeQuery,
    updatedConfiguration
  );

  PubSub.internetGameConfigurationChanged(pubSub);
  return true;
}
