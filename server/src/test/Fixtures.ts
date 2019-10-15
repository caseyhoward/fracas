import * as Models from "../repositories/Models";
import * as Uuid from "../Uuid";

export interface InternetGameConfigurationOptions {
  mapId: number;
  joinToken?: string;
  players?: Models.PlayerConfiguration[];
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
  id: number;
  name?: string;
}

export function player(options: PlayerOptions): Models.PlayerConfiguration {
  return {
    __typename: "PlayerConfiguration",
    name: options.name || "Player " + Uuid.generate(),
    color: { __typename: "Color", red: 0, green: 0, blue: 0 },
    playerId: options.id
  };
}
