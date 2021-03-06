import * as Models from "../repositories/Models";
import * as Uuid from "../Uuid";
import * as Color from "../models/Color";
import * as Player from "../models/Player";

export interface InternetGameConfigurationOptions {
  mapId: Models.MapId;
  joinToken?: string;
  players?: Player.PlayerConfiguration[];
}

export type MapOptions = {
  name?: string;
  countries?: Array<Models.Country>;
  bodiesOfWater?: Array<Models.BodyOfWater>;
  dimensions?: Models.Dimensions;
};

export function internetGameConfiguration(
  options: InternetGameConfigurationOptions
): Models.NewInternetGameConfiguration {
  return {
    __typename: "NewInternetGameConfiguration",
    players: [],
    mapId: options.mapId,
    joinToken: options.joinToken || Uuid.generate()
  };
}

export function map(options: MapOptions): Models.Map {
  return {
    id: Models.userMapId(Uuid.generate()),
    name: options.name || "Map " + Uuid.generate(),
    countries: [],
    bodiesOfWater: [],
    dimensions: { width: 0, height: 0 }
  };
}

export const playerConfiguration: Player.PlayerConfiguration = {
  __typename: "PlayerConfiguration",
  name: "Player " + Uuid.generate(),
  color: Color.black,
  playerId: Uuid.generate()
};

export function internetGame(id: string): Models.InternetGame {
  return {
    __typename: "InternetGame",
    players: [],
    mapId: Models.mapId("1", "user"),
    id,
    playerTurn: {
      __typename: "PlayerTurn",
      playerId: "1",
      playerTurnStage: Models.PlayerTurnStage.CapitolPlacement
    },
    neutralCountryTroops: []
  };
}

export function internetGameWithoutMap(
  id: string
): Models.InternetGameWithoutMap {
  return {
    __typename: "InternetGameWithoutMap",
    players: [],
    id,
    playerTurn: {
      __typename: "PlayerTurn",
      playerId: "1",
      playerTurnStage: Models.PlayerTurnStage.CapitolPlacement
    },
    neutralCountryTroops: []
  };
}

export function internetGamePlayer(
  id: string,
  gameId: string
): Models.InternetGamePlayer {
  return {
    id: id,
    gameId: gameId,
    playerToken: Uuid.generate()
  };
}

export const player: Models.Player = {
  __typename: "Player",
  id: Uuid.generate(),
  name: "Player " + Uuid.generate(),
  countryTroopCounts: [],
  color: Color.black,
  ports: []
};
