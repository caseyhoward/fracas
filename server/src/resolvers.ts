import * as database from "./db/index";
import * as InternetGame from "./InternetGame";
import * as Map from "./Map";

interface NewGame {
  mapId: string;
}

interface Game {
  id: string;
  mapId: string;
}

function gameToJson(gameRow: any): Game {
  const gameWithoutId = JSON.parse(gameRow.game_json);
  return {
    ...gameWithoutId,
    id: gameRow.id
  };
}

export const resolvers = {
  Query: {
    map: async (_: any, x: { id: string }) => {
      return Map.findById(database.query, x.id);
    },
    game: async (_: any, x: { id: string }) => {
      try {
        const result = await database.query(
          "SELECT * FROM games WHERE id = $1",
          [x.id]
        );
        return gameToJson(result.rows[0]);
      } catch (error) {
        console.log(error);
      }
    },
    maps: async (_: any, x: { id: string }) => Map.findAll(database.query)
  },
  Game: {
    map: async (game: Game) => {
      return await Map.findById(database.query, game.mapId);
    }
  },
  Mutation: {
    createMap: async (_: any, x: { map: Map.NewMap }): Promise<Map.Map> => {
      return Map.create(database.query, x.map);
    },
    createInternetGame: async (
      _: any,
      x: {}
    ): Promise<InternetGame.PlayerToken> => {
      return createInternetGameResolver(() =>
        InternetGame.create(database.query)
      );
    },
    createGame: async (
      _: any,
      x: { newGame: NewGame }
    ): Promise<Game | string> => {
      try {
        const result = await database.query(
          "INSERT INTO games(map_id, game_json) VALUES ($1, $2) RETURNING *",
          [parseInt(x.newGame.mapId, 10), JSON.stringify(x.newGame)]
        );
        return gameToJson(result.rows[0]);
      } catch (error) {
        console.log(error);
        return error.toString();
      }
    },

    saveGame: async (_: any, x: { game: Game }): Promise<Game | string> => {
      try {
        const result = await database.query(
          "UPDATE games SET game_json = $2 WHERE id = $1 RETURNING *",
          [parseInt(x.game.id, 10), JSON.stringify(x.game)]
        );
        return gameToJson(result.rows[0]);
      } catch (error) {
        console.log(error);
        return error.toString();
      }
    }
  }
};

export async function createInternetGameResolver(
  createInternetGame: () => Promise<InternetGame.PlayerToken>
) {
  return await createInternetGame();
}
