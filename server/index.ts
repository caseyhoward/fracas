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
        // console.log(x);
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
        console.log(x);
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
      console.log("Game.map : game = " + JSON.stringify(game));
      const map = await getMap(game.mapId);
      console.log("map: " + JSON.stringify(map));
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
          [parseInt(x.newGame.mapId, 10), x.newGame.gameJson]
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
  gameJson: string;
}

interface Game {
  id: String;
  mapId: string;
  gameJson: string;
}

function gameToJson(gameRow: any): Game {
  return {
    id: gameRow.id,
    mapId: gameRow.map_id,
    gameJson: gameRow.game_json
  };
}

function mapToJson(mapRow: any): Map {
  const mapWithoutId = JSON.parse(mapRow.map_json);
  return { ...mapWithoutId, id: mapRow.id };
}
