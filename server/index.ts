import { GraphQLServer } from "graphql-yoga";
import * as fs from "fs";
import { resolvers } from "./src/resolvers";

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
