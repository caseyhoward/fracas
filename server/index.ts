import { GraphQLServer, Options } from "graphql-yoga";
import * as fs from "fs";
import { resolvers } from "./src/resolvers";
import * as PubSub from "./src/PubSub";
import * as Database from "./src/Database";
import { Client, ClientConfig } from "pg";
import { PubSub as PS } from "graphql-subscriptions";

const postgresConfiguration: ClientConfig = {
  host: process.env.PGHOST,
  database: process.env.PGDATABASE,
  user: process.env.PGUSER,
  password: process.env.PGPASSWORD,
  port: parseInt(process.env.PGPORT || "5432", 10)
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
