import { GraphQLServer, Options } from "graphql-yoga";
import * as fs from "fs";
import { resolvers } from "./src/resolvers";
import * as PubSub from "./src/PubSub";
import * as Database from "./src/Database";
import { Client, ClientConfig } from "pg";
import { PubSub as PS } from "graphql-subscriptions";

// PGHOST='localhost'
// PGUSER=process.env.USER
// PGDATABASE=process.env.USER
// PGPASSWORD=null
// PGPORT=5432

const postgresConfiguration: ClientConfig = {
  user: "fracas",
  host: "localhost",
  database: "fracas",
  password: "abc123",
  port: 5432
};

async function startServer() {
  const postgresDatabase = await Database.postgres(postgresConfiguration);
  const executeQuery = postgresDatabase.query.bind(postgresDatabase);
  const pubSub: PubSub.PubSub = PubSub.inMemory();
  // const pubSub: PubSub.PubSub = PubSub.postgres(postgresConfiguration);

  fs.readFile("schema.graphql", async (_, typeDefsData) => {
    const options: Options = {
      port: 4000,
      bodyParserOptions: { limit: "50mb", type: "application/json" },
      subscriptions: {
        path: "/",
        onConnect: (_: any, __: any, ___: any) => {
          console.log("Connected");
        },
        onDisconnect: (_: any, __: any) => {
          console.log("Disconnected");
          // ...
        }
      }
    };

    const typeDefs = typeDefsData.toString("utf-8");
    const server: GraphQLServer = new GraphQLServer({
      typeDefs,
      resolvers: resolvers(executeQuery, pubSub)
    });
    await server.start(options, () =>
      console.log("Server is running on localhost:4000")
    );
  });
}

startServer();
