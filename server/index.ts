import { GraphQLServer } from "graphql-yoga";
import * as fs from "fs";
import * as database from "./db/index";

const resolvers = {
  Query: {
    map: async (_: any, x: { id: string }) => {
      try {
        // console.log(x);
        const result = await database.query(
          "SELECT * FROM maps WHERE id = $1*",
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
          "SELECT * FROM games WHERE id = $1*",
          [x.id]
        );
        console.log(result);
        return result.rows[0];
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
  Mutation: {
    createMap: async (_: any, x: { map: NewMap }): Promise<Map | string> => {
      try {
        const result = await database.query(
          "INSERT INTO maps(name, map_json) VALUES ($1, $2) RETURNING *",
          [x.map.name, x.map.mapJson]
        );
        return mapToJson(result.rows[0]);
      } catch (error) {
        console.log(error);
        return error.toString();
      }
    },

    createGame: async (
      _: any,
      x: { gameConfiguration: NewGame }
    ): Promise<Game | string> => {
      try {
        const result = await database.query(
          "INSERT INTO games(map_id, game_json) VALUES ($1, $2) RETURNING *",
          [
            parseInt(x.gameConfiguration.mapId, 10),
            x.gameConfiguration.gameJson
          ]
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
  const typeDefs = typeDefsData.toString("utf-8");
  const server = new GraphQLServer({ typeDefs, resolvers });
  server.start(() => console.log("Server is running on localhost:4000"));
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
    mapId: gameRow.mapId,
    gameJson: gameRow.game_json
  };
}

function mapToJson(mapRow: any): Map {
  return {
    id: mapRow.id,
    name: mapRow.name,
    mapJson: mapRow.map_json
  };
}
