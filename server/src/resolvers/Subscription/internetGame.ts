import * as Database from "../../Database";
import * as Graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as PubSub from "../../PubSub";

export const INTERNET_GAME_CHANGED = "INTERNET_GAME_CHANGED";

export default function internetGame(
  executeQuery: Database.ExecuteQuery,
  pubSub: PubSub.PubSub
): Graphql.SubscriptionResolverObject<
  Graphql.ResolverTypeWrapper<Graphql.Game>,
  {},
  any,
  Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
> {
  return {
    subscribe: buildSubscribe(pubSub),
    resolve: buildResolve(executeQuery)
  };
}

function buildSubscribe(pubSub: PubSub.PubSub) {
  const subscribe: Graphql.SubscriptionSubscribeFn<
    any,
    {},
    any,
    Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
  > = (_, input) => {
    console.log("subscribing", input);
    return (<any>pubSub).asyncIterator(INTERNET_GAME_CHANGED);
  };

  return subscribe;
}

function buildResolve(executeQuery: Database.ExecuteQuery) {
  const resolve: Graphql.SubscriptionResolveFn<
    Graphql.ResolverTypeWrapper<Graphql.Game>,
    any,
    any,
    Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
  > = async (_, input): Promise<Graphql.Game> => {
    console.log("resolve");
    const player = await InternetGamePlayerRepository.findByToken(
      executeQuery,
      input.playerToken
    );

    const internetGame = await InternetGameRepository.findById(
      executeQuery,
      player.gameId
    );

    return Models.internetGameToGraphql(internetGame);
  };

  return resolve;
}
