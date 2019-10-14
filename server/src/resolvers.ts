import * as Database from "./Database";
import * as InternetGameRepository from "./repositories/InternetGameRepository";
import * as InternetGameConfigurationRepository from "./repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "./repositories/InternetGamePlayerRepository";
import * as Game from "./Game";
import * as Map from "./Map";
import * as Uuid from "./Uuid";
import * as Models from "./repositories/Models";
import * as graphql from "./api/graphql";

import {
  Resolvers,
  MutationUpdateMapForInternetGameArgs,
  RequireFields,
  MutationJoinInternetGameArgs,
  InternetGamePlayerConfiguration
} from "./api/graphql";

import { createInternetGame } from "./resolvers/Mutation/createInternetGame";
import internetGame from "./resolvers/Query/internetGame";

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
      createMap: async (_, mapInput) => {
        return Map.create(executeQuery, mapInput.map);
      },
      createInternetGame: () => createInternetGame(executeQuery),
      updateMapForInternetGame,
      createGame: async (_, createGame): Promise<Game.Game> => {
        return Game.create(executeQuery, createGame.newGame);
      },
      // joinInternetGame,
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
    await InternetGameRepository.updateMap(
      executeQuery,
      player.gameId.toString(),
      parseInt(input.mapId, 10)
    );
    throw "todo";
    // return await InternetGameRepository.findById(executeQuery, player.gameId);
  }

  async function joinInternetGame(
    //   _: any,
    input: RequireFields<MutationJoinInternetGameArgs, "joinGameToken">
  ): Promise<string> {
    throw "todo";
    //   const playerToken = Uuid.generate();
    //   const internetGame = await InternetGameConfigurationRepository.findByJoinToken(
    //     executeQuery,
    //     input.joinGameToken
    //   );
    //   const newPlayer = await InternetGamePlayerRepository.create(
    //     executeQuery,
    //     internetGame.id,
    //     playerToken
    //   );
    //   const updatedPlayers: InternetGamePlayerConfiguration[] = [
    //     ...internetGame.players,
    //     {
    //       __typename: "InternetGamePlayerConfiguration",
    //       playerId: newPlayer.id,
    //       name: "",
    //       color: { red: 0, green: 0, blue: 0 }
    //     }
    //   ];
    //   const updatedGame: Models.InternetGame = {
    //     ...internetGame,
    //     players: updatedPlayers
    //   };
    //   InternetGameRepository.save(executeQuery, updatedGame);

    //   return playerToken;
  }

  async function gameMapResolver(game: Game.Game) {
    const gameWithMapId = await Game.get(executeQuery, parseInt(game.id, 10));
    return Map.findById(executeQuery, gameWithMapId.id);
  }
}
