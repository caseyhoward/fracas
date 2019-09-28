import { GraphQLServer } from "graphql-yoga";
import * as fs from "fs";

const resolvers = {
  Query: {
    map: (_: any, x: { id: string }) => {
      return `Hello ${x.id || "World"}`;
    }
  },
  Mutation: {
    createMap: (_: any, x: {}) => {
      console.log(x);
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
  legacy_raw_map: string;
  countries: Country[];
  width: number;
  height: number;
  neighboringCountries: { countryId1: string; countryId2: string }[];
  countriesAdjacentToWater: { countryId: String; countryId2: string }[];
}

interface Country {
  id: string;
  polygon: Point[];
  waterEdges: {
    point1: Point;
    point2: Point;
  }[];
}

interface Point {
  x: number;
  y: number;
}
