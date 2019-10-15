import * as Database from "./Database";
import * as InternetGamePlayerRepository from "./repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "./repositories/InternetGameConfigurationRepository";
import * as Game from "./Game";
import * as Map from "./repositories/MapRepository";
import * as graphql from "./api/graphql";

import {
  Resolvers,
  MutationUpdateMapForInternetGameArgs,
  RequireFields
} from "./api/graphql";

import { createInternetGame } from "./resolvers/Mutation/createInternetGame";
import internetGame from "./resolvers/Query/internetGame";
import joinInternetGame from "./resolvers/Mutation/joinInternetGame";
import updatePlayerName from "./resolvers/Mutation/updatePlayerName";

export function resolvers(executeQuery: Database.ExecuteQuery): Resolvers {
  return {
    Query: {
      map: async (_, map) => {
        return Map.findById(executeQuery, map.id);
      },
      game: async (_, game) => {
        return Game.get(executeQuery, parseInt(game.id, 10));
      },
      maps: async () => {
        return Map.findAll(executeQuery);
      },
      internetGame: async (_, game) => internetGame(executeQuery, game)
    },
    Game: {
      map: gameMapResolver
    },
    Mutation: {
      createGame: async (_, createGame): Promise<Game.Game> => {
        return Game.create(executeQuery, createGame.newGame);
      },
      createMap: async (_, mapInput) => {
        return Map.create(executeQuery, mapInput.map);
      },
      joinInternetGame: (_, input) => joinInternetGame(executeQuery, input),
      createInternetGame: () => createInternetGame(executeQuery),
      updatePlayerNameForInternetGame: (_, input) =>
        updatePlayerName(executeQuery, input),
      updateMapForInternetGame,
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
  ): Promise<graphql.InternetGameConfiguration> {
    const player = await InternetGamePlayerRepository.findByToken(
      executeQuery,
      input.playerToken
    );
    await InternetGameConfigurationRepository.updateMap(
      executeQuery,
      player.gameId.toString(),
      parseInt(input.mapId, 10)
    );
    throw "todo";
    // return await InternetGameRepository.findById(executeQuery, player.gameId);
  }

  async function gameMapResolver(game: graphql.Game) {
    return Map.findById(executeQuery, game.map.id);
  }
}
