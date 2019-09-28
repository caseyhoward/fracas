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
        console.log(result.rows[0]);
        return result.rows[0];
      } catch (error) {
        console.log(error);
      }
    },
    maps: async (_: any, x: { id: string }) => {
      try {
        const result = await database.query("SELECT * FROM maps");
        console.log(result.rows);
        return result.rows.map(toJson);
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
        console.log(result.rows[0]);
        return toJson(result.rows[0]);
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

function toJson(mapRow: any) {
  return {
    id: mapRow.id,
    name: mapRow.name,
    mapJson: mapRow.map_json
  };
}

interface Map {
  id: string;
  name: string;
  mapJson: string;
}

interface NewMap {
  name: string;
  mapJson: string;
}
