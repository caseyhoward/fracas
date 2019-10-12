import * as database from "./db/index";
import * as InternetGame from "./InternetGame";
import * as Map from "./Map";
import { Resolvers, Game } from "./api/graphql";

function gameToJson(gameRow: any): Game {
  const gameWithoutId = JSON.parse(gameRow.game_json);
  return {
    ...gameWithoutId,
    id: gameRow.id
  };
}

export const resolvers: Resolvers = {
  Query: {
    map: async (_, map) => {
      return Map.findById(database.query, map.id);
    },
    game: async (_, game) => {
      const result = await database.query("SELECT * FROM games WHERE id = $1", [
        game.id
      ]);
      return gameToJson(result.rows[0]);
    },
    maps: async () => Map.findAll(database.query)
  },
  Game: {
    map: async (game: any) => {
      return await Map.findById(database.query, game.mapId);
    }
  },
  Mutation: {
    createMap: async (_, mapInput) => {
      return Map.create(database.query, mapInput.map);
    },
    createInternetGame: async () => {
      return createInternetGameResolver(() =>
        InternetGame.create(database.query)
      );
    },
    createGame: async (_, x): Promise<Game> => {
      const result = await database.query(
        "INSERT INTO games(map_id, game_json) VALUES ($1, $2) RETURNING *",
        [parseInt(x.newGame.mapId, 10), JSON.stringify(x.newGame)]
      );
      return gameToJson(result.rows[0]);
    },

    saveGame: async (_, x) => {
      const result = await database.query(
        "UPDATE games SET game_json = $2 WHERE id = $1 RETURNING *",
        [parseInt(x.game.id, 10), JSON.stringify(x.game)]
      );
      return gameToJson(result.rows[0]);
    }
  }
};

export async function createInternetGameResolver(
  createInternetGame: () => Promise<string>
) {
  return await createInternetGame();
}
