import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as Graphql from "../../api/graphql";
import * as Database from "../../Database";
import * as PubSub from "../../PubSub";

export type UpdateMapForInternetGame = () => Promise<boolean>;

export type UpdateMapForInternetGameConstructor = (
  findInternetGamePlayerByToken: InternetGamePlayerRepository.FindByToken,
  updateMapForInternetGame: InternetGameConfigurationRepository.UpdateMap,
  pubsub: PubSub.PubSub,
  input: Graphql.RequireFields<
    Graphql.MutationUpdateMapForInternetGameArgs,
    "mapId" | "playerToken"
  >
) => UpdateMapForInternetGame;

type UpdateMapForInternetGameExecutor = (
  findInternetGamePlayerByToken: InternetGamePlayerRepository.FindByToken,
  updateMapForInternetGame: InternetGameConfigurationRepository.UpdateMap,
  pubSub: PubSub.PubSub,
  playerToken: string,
  mapId: string
) => Promise<boolean>;

export const updateMapForInternetGame: UpdateMapForInternetGameConstructor = (
  findInternetGamePlayerByToken: InternetGamePlayerRepository.FindByToken,
  updateMapForInternetGame: InternetGameConfigurationRepository.UpdateMap,
  pubSub: PubSub.PubSub,
  input: Graphql.RequireFields<
    Graphql.MutationUpdateMapForInternetGameArgs,
    "mapId" | "playerToken"
  >
) => {
  return () => {
    return executUpdateMapForInternetGame(
      findInternetGamePlayerByToken,
      updateMapForInternetGame,
      pubSub,
      input.playerToken,
      input.mapId
    );
  };
};

const executUpdateMapForInternetGame: UpdateMapForInternetGameExecutor = async (
  findInternetGamePlayerByToken,
  updateMapForInternetGame,
  pubSub,
  playerToken,
  mapId
) => {
  const player = await findInternetGamePlayerByToken(playerToken);
  await updateMapForInternetGame(player.gameId.toString(), mapId);
  return true;
};
