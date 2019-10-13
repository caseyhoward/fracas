import * as database from "./Database";
import * as InternetGame from "./InternetGame";
import * as InternetGamePlayer from "./InternetGamePlayer";
import * as Game from "./Game";
import * as Map from "./Map";
import {
  Resolvers,
  NewGameInput,
  MutationUpdateMapForInternetGameArgs,
  RequireFields,
  InternetGameConfiguration
} from "./api/graphql";

export function resolvers(executeQuery: database.ExecuteQuery): Resolvers {
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
      internetGame: async (_, game) => {
        return InternetGame.findByPlayerToken(executeQuery, game.playerToken);
      }
    },
    Game: {
      map: gameMapResolver
    },
    Mutation: {
      createMap: async (_, mapInput) => {
        return Map.create(executeQuery, mapInput.map);
      },
      createInternetGame: async () => {
        return InternetGame.create(executeQuery);
      },
      updateMapForInternetGame,
      createGame: async (_, createGame): Promise<Game.Game> => {
        return Game.create(executeQuery, createGame.newGame);
      },
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
  ): Promise<InternetGame.InternetGame> {
    console.log("hello");
    const player = await InternetGamePlayer.findByToken(
      executeQuery,
      input.playerToken
    );
    await InternetGame.updateMap(
      executeQuery,
      player.gameId.toString(),
      parseInt(input.mapId, 10)
    );
    return await InternetGame.findById(executeQuery, player.gameId);
  }

  async function gameMapResolver(game: Game.Game) {
    const gameWithMapId = await Game.get(executeQuery, parseInt(game.id, 10));
    return Map.findById(executeQuery, gameWithMapId.id);
  }
}
