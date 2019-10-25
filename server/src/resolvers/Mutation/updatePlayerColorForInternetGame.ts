import * as Database from "../../Database";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as Graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";
import * as Color from "../../models/Color";

export default async function updatePlayerNameForInternetGame(
  executeQuery: Database.ExecuteQuery,
  input: Graphql.RequireFields<
    Graphql.MutationUpdatePlayerColorForInternetGameArgs,
    "color" | "playerToken"
  >
): Promise<boolean> {
  const internetGamePlayer = await InternetGamePlayerRepository.findByToken(
    executeQuery
  )(input.playerToken);
  const configuration = await InternetGameConfigurationRepository.findById(
    executeQuery
  )(internetGamePlayer.gameId);
  const updatedConfiguration: Models.InternetGameConfiguration = {
    ...configuration,
    players: configuration.players.map(player => {
      if (player.playerId == internetGamePlayer.id) {
        return { ...player, color: Color.fromColorInput(input.color) };
      } else {
        return player;
      }
    })
  };
  await InternetGameConfigurationRepository.save(
    executeQuery,
    updatedConfiguration
  );
  return true;
}
