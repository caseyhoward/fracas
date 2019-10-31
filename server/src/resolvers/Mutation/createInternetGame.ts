import * as Uuid from "../../Uuid";
import { ExecuteQuery } from "../../Database";
import * as Map from "../../repositories/MapRepository";
import * as Player from "../../models/Player";
import * as Models from "../../repositories/Models";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

export async function createInternetGame(
  executeQuery: ExecuteQuery
): Promise<string> {
  const hostToken = Uuid.generate();
  const joinToken = Uuid.generate();

  const internetGameId = await InternetGameConfigurationRepository.create(
    executeQuery,
    {
      __typename: "NewInternetGameConfiguration",
      joinToken: joinToken,
      mapId: Models.mapId("1", "default"),
      players: []
    }
  );

  const hostGamePlayer: Models.InternetGamePlayer = await InternetGamePlayerRepository.create(
    executeQuery,
    internetGameId,
    hostToken
  );

  await InternetGameConfigurationRepository.addPlayer(
    executeQuery,
    internetGameId,
    Player.createHost(hostGamePlayer.id)
  );

  return hostToken;
}
