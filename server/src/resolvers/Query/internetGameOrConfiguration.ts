import * as Database from "../../Database";
import * as graphql from "../../api/graphql";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as Models from "../../repositories/Models";
import * as Player from "../../models/Player";

export default async function internetGame(
  executeQuery: Database.ExecuteQuery,

  game: graphql.RequireFields<graphql.QueryInternetGameArgs, "playerToken">
): Promise<graphql.InternetGameOrConfiguration> {
  const player = await InternetGamePlayerRepository.findByToken(executeQuery)(
    game.playerToken
  );
  try {
    const configuration = await InternetGameConfigurationRepository.findById(
      executeQuery
    )(player.gameId);
    return Models.internetGameConfigurationToGraphQl(player, configuration);
  } catch (error) {
    const internetGame = await InternetGameRepository.findById(executeQuery)(
      player.gameId
    );

    const graphqlGame: graphql.Game = Models.internetGameToGraphql(
      internetGame
    );

    return {
      __typename: "InternetGame",
      game: graphqlGame,
      currentUserPlayerId: player.id
    };
  }
}
