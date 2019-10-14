import * as Database from "../../Database";
import * as graphql from "../../api/graphql";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

export default async function internetGame(
  executeQuery: Database.ExecuteQuery,
  game: graphql.RequireFields<graphql.QueryInternetGameArgs, "playerToken">
): Promise<graphql.InternetGame> {
  const player = await InternetGamePlayerRepository.findByToken(
    executeQuery,
    game.playerToken
  );
  const configuration = await InternetGameConfigurationRepository.findById(
    executeQuery,
    player.gameId
  );
  return {
    id: configuration.id,
    players: configuration.players.map(player => {
      return { ...player, __typename: "InternetGamePlayerConfiguration" };
    }),
    mapId: configuration.mapId.toString(),
    joinToken: configuration.joinToken,
    userPlayerId: player.id
  };
}
