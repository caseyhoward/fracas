import * as Models from "../repositories/Models";
import * as Uuid from "../Uuid";

interface InternetGameConfigurationOptions {
  mapId: number;
  joinToken?: string;
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
