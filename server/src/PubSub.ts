import * as GraphqlYoga from "graphql-yoga";
import * as Models from "./repositories/Models";
import * as PubSub from "./PubSub";
// import PostgresPubSub from "@udia/graphql-postgres-subscriptions";
// import * as Postgres from "pg";

const INTERNET_GAME_CHANGED = "INTERNET_GAME_CHANGED";
const INTERNET_GAME_CONFIGURATION_CHANGED =
  "INTERNET_GAME_CONFIGURATION_CHANGED";

export type PubSub =
  // | PostgresPubSub
  GraphqlYoga.PubSub | GraphqlYoga.PubSub;

export function inMemory(): PubSub {
  return new GraphqlYoga.PubSub();
}

// export function postgres(postgresConfiguration: Postgres.ClientConfig): PubSub {
//   const client = new Postgres.Client(postgresConfiguration);
//   return new PostgresPubSub(client);
// }

export function internetGameChanged(
  pubSub: PubSub.PubSub,
  internetGame: Models.InternetGameWithoutMap,
  playerId: string
) {
  const message = {
    internetGame: Models.internetGameWithoutMapToGraphql(internetGame, playerId)
  };
  pubSub.publish(INTERNET_GAME_CHANGED, message);
}

export function internetGameChangedIterator(
  pubSub: PubSub.PubSub
): AsyncIterator<void> {
  return (<any>pubSub).asyncIterator(INTERNET_GAME_CHANGED);
}

export function internetGameConfigurationChanged(pubSub: PubSub.PubSub) {
  pubSub.publish(INTERNET_GAME_CONFIGURATION_CHANGED, {});
}

export function internetGameConfigurationChangedIterator(
  pubSub: PubSub.PubSub
): AsyncIterator<void> {
  return (<any>pubSub).asyncIterator(INTERNET_GAME_CONFIGURATION_CHANGED);
}
