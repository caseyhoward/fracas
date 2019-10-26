import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as Graphql from "../../api/graphql";
import * as Database from "../../Database";
import { PubSub } from "../../PubSub";

export type UpdateMapForInternetGame = (
  playerToken: string,
  mapId: string
) => Promise<boolean>;

export type UpdateMapForInternetGameConstructor = (
  executeQuery: Database.ExecuteQuery,
  pubsub: PubSub,
  input: Graphql.RequireFields<
    Graphql.MutationUpdateMapForInternetGameArgs,
    "mapId" | "playerToken"
  >
) => UpdateMapForInternetGame;

type UpdateMapForInternetGameExecutor = (
  executeQuery: Database.ExecuteQuery,
  playerToken: string,
  mapId: string
) => Promise<boolean>;

export const updateMapForInternetGame: UpdateMapForInternetGameConstructor = (
  executeQuery: Database.ExecuteQuery,
  pubsub: PubSub
) => {
  return (playerToken, mapId) => {
    return executUpdateMapForInternetGame(executeQuery, playerToken, mapId);
  };
};

const executUpdateMapForInternetGame: UpdateMapForInternetGameExecutor = async (
  executeQuery,
  playerToken,
  mapId
) => {
  const player = await InternetGamePlayerRepository.findByToken(executeQuery)(
    playerToken
  );
  await InternetGameConfigurationRepository.updateMap(executeQuery)(
    player.gameId.toString(),
    mapId
  );
  return true;
};
