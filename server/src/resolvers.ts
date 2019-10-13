import * as database from "./db/index";
import * as InternetGame from "./InternetGame";
import * as Game from "./Game";
import * as Map from "./Map";
import { Resolvers, NewGameInput } from "./api/graphql";

export const resolvers: Resolvers = {
  Query: {
    map: async (_, map) => {
      return Map.findById(database.query, map.id);
    },
    game: async (_, game) => {
      return Game.get(database.query, parseInt(game.id, 10));
    },
    maps: async () => Map.findAll(database.query),
    internetGame: async (_, game) => {
      return InternetGame.findByPlayerToken(database.query, game.playerToken);
    }
  },
  Game: {
    map: game => Map.findById(database.query, game.map.id)
  },
  Mutation: {
    createMap: async (_, mapInput) => {
      return Map.create(database.query, mapInput.map);
    },
    createInternetGame: async () => {
      return InternetGame.create(database.query);
    },
    createGame: async (_, createGame): Promise<Game.Game> => {
      return Game.create(database.query, createGame.newGame);
    },
    saveGame: async (_, saveGame) => {
      return Game.update(database.query, saveGame.game);
    }
  }
};
