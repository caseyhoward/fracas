import PostgresPubSub from "@udia/graphql-postgres-subscriptions";
import { PubSub as PS } from "graphql-yoga";
import * as Postgres from "pg";

export const INTERNET_GAME_CHANGED = "INTERNET_GAME_CHANGED";
export const INTERNET_GAME_CONFIGURATION_CHANGED =
  "INTERNET_GAME_CONFIGURATION_CHANGED";

export type PubSub = PostgresPubSub | PS;

export function inMemory(): PubSub {
  return new PS();
}

export function postgres(postgresConfiguration: Postgres.ClientConfig): PubSub {
  const client = new Postgres.Client(postgresConfiguration);
  return new PostgresPubSub(client);
}
