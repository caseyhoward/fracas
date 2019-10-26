import PostgresPubSub from "@udia/graphql-postgres-subscriptions";
import { PubSub as PS } from "graphql-yoga";
import * as Postgres from "pg";
import * as Models from "./repositories/Models";
import * as PubSub from "./PubSub";

const INTERNET_GAME_CHANGED = "INTERNET_GAME_CHANGED";
const INTERNET_GAME_CONFIGURATION_CHANGED =
  "INTERNET_GAME_CONFIGURATION_CHANGED";

export type PubSub = PostgresPubSub | PS;

export function inMemory(): PubSub {
  return new PS();
}

export function postgres(postgresConfiguration: Postgres.ClientConfig): PubSub {
  const client = new Postgres.Client(postgresConfiguration);
  return new PostgresPubSub(client);
}

export function internetGameChanged(
  pubSub: PubSub.PubSub,
  internetGame: Models.InternetGame,
  playerId: string
) {
  const message = {
    internetGame: Models.internetGameToGraphql(internetGame, playerId)
  };
  pubSub.publish(INTERNET_GAME_CHANGED, message);
}

export function internetGameChangedIterator(
  pubSub: PubSub.PubSub
): AsyncIterator<void> {
  return (<any>pubSub).asyncIterator(INTERNET_GAME_CHANGED);
}

export function internetGameConfigurationChanged(
  pubSub: PubSub.PubSub,
  configuration: Models.InternetGameConfiguration,
  internetGamePlayer: Models.InternetGamePlayer
) {
  const message = {
    internetGameOrConfiguration: Models.internetGameConfigurationToGraphQl(
      internetGamePlayer,
      configuration
    )
  };
  pubSub.publish(INTERNET_GAME_CONFIGURATION_CHANGED, message);
}

export function internetGameConfigurationChangedIterator(
  pubSub: PubSub.PubSub
): AsyncIterator<void> {
  return (<any>pubSub).asyncIterator(INTERNET_GAME_CONFIGURATION_CHANGED);
}
