import * as Graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as PubSub from "../../PubSub";

export function buildSubscribe(pubSub: PubSub.PubSub) {
  const subscribe: Graphql.SubscriptionSubscribeFn<
    any,
    {},
    any,
    Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
  > = (_, input) => {
    return (<any>pubSub).asyncIterator(PubSub.INTERNET_GAME_CHANGED);
  };

  return subscribe;
}

export function buildResolve(
  findPlayerByToken: InternetGamePlayerRepository.FindByToken,
  findGameById: InternetGameRepository.FindById
) {
  const resolve: Graphql.SubscriptionResolveFn<
    Graphql.ResolverTypeWrapper<Graphql.Game>,
    any,
    any,
    Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
  > = async (_, input): Promise<Graphql.Game> => {
    const player = await findPlayerByToken(input.playerToken);
    const internetGame = await findGameById(player.gameId);
    return Models.internetGameToGraphql(internetGame, player.id);
  };

  return resolve;
}
