import * as Uuid from "../../Uuid";
import { ExecuteQuery } from "../../Database";
import * as Map from "../../Map";
import * as Models from "../../repositories/Models";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";

import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

export async function createInternetGame(
  executeQuery: ExecuteQuery
): Promise<string> {
  await executeQuery("BEGIN");

  const mapId = await Map.findFirstId(executeQuery);
  const hostToken = Uuid.generate();
  const joinToken = Uuid.generate();

  const internetGameId = await InternetGameConfigurationRepository.create(
    executeQuery,
    {
      __typename: "NewInternetGameConfiguration",
      joinToken: joinToken,
      mapId: mapId,
      players: []
    }
  );

  const hostGamePlayer: Models.InternetGamePlayer = await InternetGamePlayerRepository.create(
    executeQuery,
    internetGameId,
    hostToken
  );

  const host: Models.PlayerConfiguration = {
    __typename: "PlayerConfiguration",
    color: { __typename: "Color", red: 0, green: 255, blue: 0 },
    name: "Host",
    playerId: hostGamePlayer.id
  };

  await InternetGameConfigurationRepository.addPlayer(
    executeQuery,
    internetGameId,
    host
  );

  await executeQuery("COMMIT");
  return hostToken;
}

export const defaultHostColor: Models.Color = {
  __typename: "Color",
  red: 0,
  green: 255,
  blue: 0
};
