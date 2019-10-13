import { GraphQLServer } from "graphql-yoga";
import * as fs from "fs";
import { resolvers } from "./src/resolvers";

import * as Database from "./src/Database";

// PGHOST='localhost'
// PGUSER=process.env.USER
// PGDATABASE=process.env.USER
// PGPASSWORD=null
// PGPORT=5432

const executePostgresQuery = Database.postgres({
  user: "fracas",
  host: "localhost",
  database: "fracas",
  password: "abc123",
  port: 5432
});
executePostgresQuery("SELECT * FROM maps;").then(x => console.log(x));
fs.readFile("schema.graphql", (error, typeDefsData) => {
  const options = {
    port: 4000,
    bodyParserOptions: { limit: "50mb", type: "application/json" }
  };

  const typeDefs = typeDefsData.toString("utf-8");
  const server = new GraphQLServer({
    typeDefs,
    resolvers: <any>resolvers(executePostgresQuery)
  });
  server.start(options, () =>
    console.log("Server is running on localhost:4000")
  );
});
