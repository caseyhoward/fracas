import PostgresPubSub from "@udia/graphql-postgres-subscriptions";
import { PubSub as PS } from "graphql-subscriptions";
import * as Postgres from "pg";

export type PubSub = PostgresPubSub | PS;

export function inMemory(): PubSub {
  return new PS();
}

export function postgres(postgresConfiguration: Postgres.ClientConfig): PubSub {
  const client = new Postgres.Client(postgresConfiguration);
  return new PostgresPubSub(client);
}
