import * as Uuid from "../../Uuid";
import { ExecuteQuery } from "../../Database";
import * as Map from "../../Map";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";

import {
  InternetGame,
  InternetGamePlayerConfiguration,
  InternetGameConfiguration,
  Player,
  CountryTroopCounts,
  PlayerTurn
} from "../../api/graphql";

export {
  Game,
  NewGameInput,
  InternetGame,
  InternetGameConfiguration
} from "../../api/graphql";

import * as InternetGamePlayer from "../../InternetGamePlayer";

export async function createInternetGame(
  executeQuery: ExecuteQuery
): Promise<string> {
  const mapId = await Map.findFirstId(executeQuery);
  const hostToken = Uuid.generate();
  const joinToken = Uuid.generate();
  // const newRowGameJson: InternetGameRepository.ConfigurationJson = {
  //   players: [],
  //   userPlayerId: 1
  // };
  // const newRow: InternetGameRepository.NewRow = {
  //   join_token: joinToken,
  //   map_id: mapId,
  //   game_json: JSON.stringify(newRowGameJson)
  // };
  const internetGameId = await InternetGameRepository.create(executeQuery, {
    __typename: "NewConfiguration",
    joinToken: joinToken,
    mapId: mapId,
    players: []
  });

  const hostGamePlayer: InternetGamePlayer.InternetGamePlayer = await InternetGamePlayer.create(
    executeQuery,
    internetGameId.toString(),
    hostToken
  );

  const host: InternetGameRepository.PlayerConfiguration = {
    color: { __typename: "Color", red: 0, green: 255, blue: 0 },
    name: "Host",
    playerId: hostGamePlayer.id
  };

  await InternetGameRepository.addPlayer(executeQuery, internetGameId, host);

  return hostToken;
}

export const defaultHostColor: InternetGameRepository.Color = {
  __typename: "Color",
  red: 0,
  green: 255,
  blue: 0
};
