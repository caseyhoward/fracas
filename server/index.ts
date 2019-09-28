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
    }
  },
  Mutation: {
    createMap: async (_: any, x: { map: NewMap }) => {
      try {
        const result = await database.query(
          "INSERT INTO maps(name, map_json) VALUES ($1, $2) RETURNING *",
          [x.map.name, x.map.mapJson]
        );
        console.log(result.rows[0]);
        return {
          id: result.rows[0].id,
          name: result.rows[0].name,
          mapJson: result.rows[0].map_json
        };
      } catch (error) {
        console.log(error);
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
