import { GraphQLServer } from "graphql-yoga";
import * as fs from "fs";
import * as database from "./db/index";
import * as express from "express";

async function getMap(id: string): Promise<Map> {
  const result = await database.query("SELECT * FROM maps WHERE id = $1", [id]);
  return result.rows[0];
}

const resolvers = {
  Query: {
    map: async (_: any, x: { id: string }) => {
      try {
        const result = await database.query(
          "SELECT * FROM maps WHERE id = $1",
          [x.id]
        );
        return result.rows[0];
      } catch (error) {
        console.log(error);
      }
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
    maps: async (_: any, x: { id: string }) => {
      try {
        const result = await database.query("SELECT * FROM maps");

        return result.rows.map(mapToJson);
      } catch (error) {
        console.log(error);
      }
    }
  },
  Game: {
    map: async (game: Game) => {
      const map = await getMap(game.mapId);
      return mapToJson(map);
    }
  },
  Mutation: {
    createMap: async (_: any, x: { map: NewMap }): Promise<Map | string> => {
      try {
        const result = await database.query(
          "INSERT INTO maps(name, map_json) VALUES ($1, $2) RETURNING *",
          [x.map.name, JSON.stringify(x.map)]
        );
        return mapToJson(result.rows[0]);
      } catch (error) {
        console.log(error);
        return error.toString();
      }
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

fs.readFile("schema.graphql", (error, typeDefsData) => {
  const options = {
    port: 4000,
    bodyParserOptions: { limit: "50mb", type: "application/json" }
  };

  const typeDefs = typeDefsData.toString("utf-8");
  const server = new GraphQLServer({ typeDefs, resolvers });
  server.start(options, () =>
    console.log("Server is running on localhost:4000")
  );
});

interface Map {
  id: string;
  name: string;
  mapJson: string;
}

interface NewMap {
  name: string;
  mapJson: string;
}

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

function mapToJson(mapRow: any): Map {
  const mapWithoutId = JSON.parse(mapRow.map_json);
  return { ...mapWithoutId, id: mapRow.id };
}
