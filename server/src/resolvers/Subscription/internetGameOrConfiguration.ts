import * as Graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as PubSub from "../../PubSub";

export function buildSubscribe(pubSub: PubSub.PubSub) {
  const subscribe: Graphql.SubscriptionSubscribeFn<
    any,
    {},
    any,
    Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
  > = () => {
    return PubSub.internetGameConfigurationChangedIterator(pubSub);
  };

  return subscribe;
}

export function buildResolve(
  findPlayerByToken: InternetGamePlayerRepository.FindByToken,
  findConfigurationById: InternetGameConfigurationRepository.FindById,
  findGameById: InternetGameRepository.FindById
) {
  const resolve: Graphql.SubscriptionResolveFn<
    Graphql.ResolverTypeWrapper<Graphql.InternetGameOrConfiguration>,
    any,
    any,
    Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
  > = async (_, input): Promise<Graphql.InternetGameOrConfiguration> => {
    const player = await findPlayerByToken(input.playerToken);
    try {
      const configuration = await findConfigurationById(player.gameId);
      return Models.internetGameConfigurationToGraphQl(player, configuration);
    } catch (error) {
      const internetGame = await findGameById(player.gameId);
      return Models.internetGameToGraphql(internetGame, player.id);
    }
  };

  return resolve;
}
