import * as Database from "../../Database";
import * as graphql from "../../api/graphql";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

export default async function internetGame(
  executeQuery: Database.ExecuteQuery,
  game: graphql.RequireFields<graphql.QueryInternetGameArgs, "playerToken">
): Promise<graphql.InternetGameOrConfiguration> {
  const player = await InternetGamePlayerRepository.findByToken(
    executeQuery,
    game.playerToken
  );
  try {
    const configuration = await InternetGameConfigurationRepository.findById(
      executeQuery,
      player.gameId
    );
    return {
      __typename: "InternetGameConfiguration",
      id: configuration.id,
      players: configuration.players.map(player => {
        return { ...player, __typename: "InternetGamePlayerConfiguration" };
      }),
      mapId: configuration.mapId.toString(),
      joinToken: configuration.joinToken,
      currentUserPlayerId: player.id
    };
  } catch (error) {
    const internetGame = await InternetGameRepository.findById(
      executeQuery,
      player.gameId
    );

    const graphqlGame: graphql.Game = {
      __typename: "Game",
      id: internetGame.id.toString(),
      map: <any>{ id: internetGame.mapId.toString() },
      neutralCountryTroops: internetGame.neutralCountryTroops,
      playerTurn: {
        ...internetGame.playerTurn,
        playerId: internetGame.playerTurn.playerId.toString()
      },
      players: internetGame.players.map(player => {
        return {
          ...player,
          id: player.id.toString(),
          __typename: "Player"
        };
      })
    };
    return {
      __typename: "InternetGame",
      game: graphqlGame,
      currentUserPlayerId: player.id
    };
  }
}
