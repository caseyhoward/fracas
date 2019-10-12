import * as database from "./db/index";
import * as InternetGame from "./InternetGame";
import * as Map from "./Map";
import { Resolvers, Game, NewGameInput } from "./api/graphql";

function gameToJson(gameRow: GameRow): Game {
  const gameWithoutId = JSON.parse(gameRow.game_json);

  return {
    ...gameWithoutId,
    id: gameRow.id,
    map: { id: gameRow.map_id }
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
    map: async game => {
      return await Map.findById(database.query, game.map.id);
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
    createGame: async (_, createGame): Promise<Game> => {
      const newGameRow = gameInputToGameRow(createGame.newGame);
      const result = await database.query(
        "INSERT INTO games(map_id, game_json) VALUES ($1, $2) RETURNING *",
        [
          parseInt(createGame.newGame.mapId, 10),
          JSON.stringify(createGame.newGame)
        ]
      );
      return gameToJson(result.rows[0]);
    },

    saveGame: async (_, saveGame) => {
      const result = await database.query(
        "UPDATE games SET game_json = $2 WHERE id = $1 RETURNING *",
        [parseInt(saveGame.game.id, 10), JSON.stringify(saveGame.game)]
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

function gameInputToGameRow(gameInput: NewGameInput): NewGameRow {
  const gameJson = {
    players: gameInput.players,
    playerTurn: gameInput.playerTurn,
    neutralCountryTroops: gameInput.neutralCountryTroops
  };

  return {
    map_id: parseInt(gameInput.mapId),
    game_json: JSON.stringify(gameJson)
  };
}

interface GameRow {
  id: number;
  map_id: number;
  game_json: string;
}

interface NewGameRow {
  map_id: number;
  game_json: string;
}
