import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as Graphql from "../../api/graphql";
import * as PubSub from "../../PubSub";
import * as Models from "../../repositories/Models";

export type UpdateMapForInternetGame = Promise<boolean>;

export type UpdateMapForInternetGameConstructor = (
  findInternetGamePlayerByToken: InternetGamePlayerRepository.FindByToken,
  updateMapForInternetGame: InternetGameConfigurationRepository.UpdateMap,
  pubsub: PubSub.PubSub,
  input: Graphql.RequireFields<
    Graphql.MutationUpdateMapForInternetGameArgs,
    "mapId" | "mapIdType" | "playerToken"
  >
) => UpdateMapForInternetGame;

type UpdateMapForInternetGameExecutor = (
  findInternetGamePlayerByToken: InternetGamePlayerRepository.FindByToken,
  updateMapForInternetGame: InternetGameConfigurationRepository.UpdateMap,
  pubSub: PubSub.PubSub,
  playerToken: string,
  mapId: string,
  mapIdType: string
) => Promise<boolean>;

export const updateMapForInternetGame: UpdateMapForInternetGameConstructor = (
  findInternetGamePlayerByToken: InternetGamePlayerRepository.FindByToken,
  updateMapForInternetGame: InternetGameConfigurationRepository.UpdateMap,
  pubSub: PubSub.PubSub,
  input: Graphql.RequireFields<
    Graphql.MutationUpdateMapForInternetGameArgs,
    "mapId" | "mapIdType" | "playerToken"
  >
) => {
  return executUpdateMapForInternetGame(
    findInternetGamePlayerByToken,
    updateMapForInternetGame,
    pubSub,
    input.playerToken,
    input.mapId,
    input.mapIdType
  );
};

const executUpdateMapForInternetGame: UpdateMapForInternetGameExecutor = async (
  findInternetGamePlayerByToken,
  updateMapForInternetGame,
  pubSub,
  playerToken,
  mapIdString,
  mapIdTypeString
) => {
  let mapIdType: "user" | "default";

  if (mapIdTypeString === "user") {
    mapIdType = "user";
  } else if (mapIdTypeString === "default") {
    mapIdType = "default";
  } else {
    throw "Unknown map id type";
  }
  const mapId: Models.MapId = Models.mapId(mapIdString, mapIdType);
  const player = await findInternetGamePlayerByToken(playerToken);
  await updateMapForInternetGame(player.gameId.toString(), mapId);
  PubSub.internetGameConfigurationChanged(pubSub);
  return true;
};
