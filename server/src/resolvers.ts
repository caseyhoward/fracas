import * as Database from "./Database";
import * as InternetGamePlayerRepository from "./repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "./repositories/InternetGameConfigurationRepository";
import * as Game from "./Game";
import * as graphql from "./api/graphql";

import {
  Resolvers,
  MutationUpdateMapForInternetGameArgs,
  RequireFields
} from "./api/graphql";

import { createInternetGame } from "./resolvers/Mutation/createInternetGame";
import internetGame from "./resolvers/Query/internetGame";
import gameMap from "./resolvers/Game/map";
import map from "./resolvers/Query/map";
import maps from "./resolvers/Query/maps";
import createMap from "./resolvers/Mutation/createMap";
import saveInternetGame from "./resolvers/Mutation/saveInternetGame";
import joinInternetGame from "./resolvers/Mutation/joinInternetGame";
import startInternetGame from "./resolvers/Mutation/startInternetGame";
import updatePlayerNameForInternetGame from "./resolvers/Mutation/updatePlayerNameForInternetGame";
import updatePlayerColorForInternetGame from "./resolvers/Mutation/updatePlayerColorForInternetGame";

export function resolvers(executeQuery: Database.ExecuteQuery): Resolvers {
  return {
    Query: {
      map: async (_, query) => map(executeQuery, query),
      game: async (_, game) => {
        return Game.get(executeQuery, parseInt(game.id, 10));
      },
      maps: async () => maps(executeQuery),
      internetGame: async (_, game) => internetGame(executeQuery, game)
    },
    Game: {
      map: (game, _) => gameMap(executeQuery, game)
    },
    Mutation: {
      createGame: async (_, createGame): Promise<Game.Game> => {
        return Game.create(executeQuery, createGame.newGame);
      },
      createMap: async (_, input) => createMap(executeQuery, input),
      joinInternetGame: (_, input) => joinInternetGame(executeQuery, input),
      createInternetGame: () => createInternetGame(executeQuery),
      startInternetGame: (_, input) => startInternetGame(executeQuery, input),
      updatePlayerNameForInternetGame: (_, input) =>
        updatePlayerNameForInternetGame(executeQuery, input),
      updatePlayerColorForInternetGame: (_, input) =>
        updatePlayerColorForInternetGame(executeQuery, input),
      updateMapForInternetGame,
      saveInternetGame: async (_, input) =>
        saveInternetGame(executeQuery, input),
      saveGame: async (_, saveGame) => {
        return Game.update(executeQuery, saveGame.game);
      }
    }
  };

  async function updateMapForInternetGame(
    _: any,
    input: RequireFields<
      MutationUpdateMapForInternetGameArgs,
      "playerToken" | "mapId"
    >
  ): Promise<boolean> {
    const player = await InternetGamePlayerRepository.findByToken(
      executeQuery,
      input.playerToken
    );
    await InternetGameConfigurationRepository.updateMap(
      executeQuery,
      player.gameId.toString(),
      parseInt(input.mapId, 10)
    );
    return true;
  }
}
