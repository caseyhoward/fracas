import * as Database from "../../Database";
import * as Graphql from "../../api/graphql";
import * as Models from "../../repositories/Models";
import * as Player from "../../models/Player";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as PubSub from "../../PubSub";

export const INTERNET_GAME_CHANGED = "INTERNET_GAME_CHANGED";

export function buildSubscribe(pubSub: PubSub.PubSub) {
  const subscribe: Graphql.SubscriptionSubscribeFn<
    any,
    {},
    any,
    Graphql.RequireFields<Graphql.SubscriptionInternetGameArgs, "playerToken">
  > = (_, input) => {
    return (<any>pubSub).asyncIterator("INTERNET_GAME_CONFIGURATION_CHANGED");
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

      const graphqlGame: Graphql.Game = Models.internetGameToGraphql(
        internetGame
      );

      return {
        __typename: "InternetGame",
        game: graphqlGame,
        currentUserPlayerId: player.id
      };
    }
  };

  return resolve;
}
