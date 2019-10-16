import * as Database from "../../Database";
import * as graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

export default async function internetGame(
  executeQuery: Database.ExecuteQuery,

  game: graphql.RequireFields<graphql.QueryInternetGameArgs, "playerToken">
): Promise<graphql.InternetGame> {
  const player = await InternetGamePlayerRepository.findByToken(
    executeQuery,
    game.playerToken
  );

  const internetGame = await InternetGameRepository.findById(
    executeQuery,
    player.gameId
  );

  const graphqlGame: graphql.Game = Models.internetGameToGraphql(internetGame);

  return {
    __typename: "InternetGame",
    game: graphqlGame,
    currentUserPlayerId: player.id
  };
}
