import * as Models from "../repositories/Models";
import * as Uuid from "../Uuid";
import * as Color from "../models/Color";
import * as Player from "../models/Player";

export interface InternetGameConfigurationOptions {
  mapId: string;
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

export function map(options: MapOptions): Models.NewMap {
  return {
    name: options.name || "Map " + Uuid.generate(),
    countries: [],
    bodiesOfWater: [],
    dimensions: { width: 0, height: 0 }
  };
}

interface PlayerOptions {
  id: string;
  name?: string;
  color?: Color.Color;
}

export function playerConfiguration(
  options: PlayerOptions
): Player.PlayerConfiguration {
  return {
    __typename: "PlayerConfiguration",
    name: options.name || "Player " + Uuid.generate(),
    color: options.color || Color.black,
    playerId: options.id
  };
}

export function internetGame(id: string): Models.InternetGame {
  return {
    __typename: "InternetGame",
    players: [],
    mapId: "1",
    id,
    playerTurn: {
      __typename: "PlayerTurn",
      playerId: "1",
      playerTurnStage: Models.PlayerTurnStage.CapitolPlacement
    },
    neutralCountryTroops: []
  };
}
