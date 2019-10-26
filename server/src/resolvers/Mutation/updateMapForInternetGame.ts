import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";

import {
  MutationUpdateMapForInternetGameArgs,
  RequireFields
} from "../../api/graphql";
import * as Database from "../../Database";

export type UpdateMapForInternetGame = (
  playerToken: string,
  mapId: string
) => Promise<boolean>;

export type UpdateMapForInternetGameConstructor = (
  executeQuery: Database.ExecuteQuery
) => UpdateMapForInternetGame;

type UpdateMapForInternetGameExecutor = (
  executeQuery: Database.ExecuteQuery,
  playerToken: string,
  mapId: string
) => Promise<boolean>;

export const updateMapForInternetGame: UpdateMapForInternetGameConstructor = (
  executeQuery: Database.ExecuteQuery
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
  await InternetGameConfigurationRepository.updateMap(
    executeQuery,
    player.gameId.toString(),
    parseInt(mapId, 10)
  );
  return true;
};
