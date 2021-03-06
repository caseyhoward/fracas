import * as Database from "./Database";
import * as InternetGamePlayerRepository from "./repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "./repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "./repositories/InternetGameRepository";
import * as Game from "./Game";
import { createInternetGame } from "./resolvers/Mutation/createInternetGame";
import internetGameOrConfiguration from "./resolvers/Query/internetGameOrConfiguration";
import internetGame from "./resolvers/Query/internetGame";
import gameMap from "./resolvers/Game/map";
import map from "./resolvers/Query/map";
import maps from "./resolvers/Query/maps";
import createMap from "./resolvers/Mutation/createMap";
import saveInternetGame from "./resolvers/Mutation/saveInternetGame";
import joinInternetGame from "./resolvers/Mutation/joinInternetGame";
import startInternetGame from "./resolvers/Mutation/startInternetGame";
import updatePlayerNameForInternetGame from "./resolvers/Mutation/updatePlayerNameForInternetGame";
import updatePlayerColorForInternetGame from "./resolvers/Mutation/updatePlayerColorForInternetGame";
import { updateMapForInternetGame } from "./resolvers/Mutation/updateMapForInternetGame";
import * as SubscriptionInternetGame from "./resolvers/Subscription/internetGame";
import * as SubscriptionInternetGameOrConfiguration from "./resolvers/Subscription/internetGameOrConfiguration";
import { IResolvers } from "graphql-tools";
import * as PubSub from "./PubSub";

export function resolvers(
  executeQuery: Database.ExecuteQuery,
  pubsub: PubSub.PubSub
): IResolvers<any, any> {
  return {
    Query: {
      map: async (_, query) => map(executeQuery, query),
      game: async (_, game) => {
        return Game.get(executeQuery, parseInt(game.id, 10));
      },
      maps: async () => maps(executeQuery),
      internetGameOrConfiguration: async (_, game) =>
        internetGameOrConfiguration(executeQuery, game),
      internetGame: async (_, game) => internetGame(executeQuery, game)
    },
    Game: {
      map: (game, _) => gameMap(executeQuery, game)
    },
    Mutation: {
      createGame: async (_, createGame): Promise<Game.Game> => {
        return Game.create(executeQuery, createGame.newGame);
      },
      createMap: async (_, input) => createMap(executeQuery, input),
      joinInternetGame: (_, input) =>
        joinInternetGame(executeQuery, pubsub, input),
      createInternetGame: () => createInternetGame(executeQuery),
      startInternetGame: (_, input) =>
        startInternetGame(executeQuery, pubsub, input),
      updatePlayerNameForInternetGame: (_, input) =>
        updatePlayerNameForInternetGame(executeQuery, pubsub, input),
      updatePlayerColorForInternetGame: (_, input) =>
        updatePlayerColorForInternetGame(executeQuery, pubsub, input),
      updateMapForInternetGame: (_, input) =>
        updateMapForInternetGame(
          InternetGamePlayerRepository.findByToken(executeQuery),
          InternetGameConfigurationRepository.updateMap(executeQuery),
          pubsub,
          input
        ),
      saveInternetGame: async (_, input) =>
        saveInternetGame(executeQuery, pubsub, input),
      saveGame: async (_, saveGame) => {
        return Game.update(executeQuery, saveGame.game);
      }
    },
    Subscription: {
      internetGameOrConfiguration: {
        resolve: SubscriptionInternetGameOrConfiguration.buildResolve(
          InternetGamePlayerRepository.findByToken(executeQuery),
          InternetGameConfigurationRepository.findById(executeQuery),
          InternetGameRepository.findById(executeQuery)
        ),
        subscribe: SubscriptionInternetGameOrConfiguration.buildSubscribe(
          pubsub
        )
      },
      internetGame: {
        resolve: SubscriptionInternetGame.buildResolve(
          InternetGamePlayerRepository.findByToken(executeQuery),
          InternetGameRepository.findById(executeQuery)
        ),
        subscribe: SubscriptionInternetGame.buildSubscribe(pubsub)
      }
    }
  };
}
