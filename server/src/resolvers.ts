import * as Database from "./Database";
import * as InternetGamePlayer from "./InternetGamePlayer";
import * as InternetGameRepository from "./repositories/InternetGameRepository";
import * as Game from "./Game";
import * as Map from "./Map";
import * as Uuid from "./Uuid";

import {
  Resolvers,
  MutationUpdateMapForInternetGameArgs,
  RequireFields,
  MutationJoinInternetGameArgs,
  InternetGamePlayerConfiguration
} from "./api/graphql";

import { createInternetGame } from "./resolvers/Mutation/createInternetGame";

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
      }
      // internetGame: async (_, game) => {
      //   return InternetGameRepository.findByPlayerToken(
      //     executeQuery,
      //     game.playerToken
      //   );
      // }
    },
    Game: {
      map: gameMapResolver
    },
    Mutation: {
      createMap: async (_, mapInput) => {
        return Map.create(executeQuery, mapInput.map);
      },
      createInternetGame: () => createInternetGame(executeQuery),
      // updateMapForInternetGame,
      createGame: async (_, createGame): Promise<Game.Game> => {
        return Game.create(executeQuery, createGame.newGame);
      },
      joinInternetGame,
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
  ): Promise<InternetGameRepository.InternetGame> {
    const player = await InternetGamePlayer.findByToken(
      executeQuery,
      input.playerToken
    );
    await InternetGameRepository.updateMap(
      executeQuery,
      player.gameId.toString(),
      parseInt(input.mapId, 10)
    );
    return await InternetGameRepository.findById(executeQuery, player.gameId);
  }

  async function joinInternetGame(
    _: any,
    input: RequireFields<MutationJoinInternetGameArgs, "joinGameToken">
  ): Promise<string> {
    const playerToken = Uuid.generate();
    const internetGame = await InternetGameRepository.findByJoinToken(
      executeQuery,
      input.joinGameToken
    );
    if (InternetGameRepository.isConfiguring(internetGame)) {
      const newPlayer = await InternetGamePlayer.create(
        executeQuery,
        internetGame.id.toString(),
        playerToken
      );
      const updatedPlayers: InternetGamePlayerConfiguration[] = [
        ...internetGame.players,
        {
          playerId: newPlayer.id,
          name: "",
          color: { red: 0, green: 0, blue: 0 }
        }
      ];
      const updatedGame: InternetGameRepository.InternetGame = {
        ...internetGame,
        players: updatedPlayers
      };
      console.log();
      InternetGameRepository.save(executeQuery, updatedGame);
    } else {
      throw "You can only join games that haven't started";
    }

    return playerToken;
  }

  async function gameMapResolver(game: Game.Game) {
    const gameWithMapId = await Game.get(executeQuery, parseInt(game.id, 10));
    return Map.findById(executeQuery, gameWithMapId.id);
  }
}
