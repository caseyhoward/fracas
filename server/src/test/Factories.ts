import * as Builders from "./Builders";
import * as InternetGameConfigurationRepository from "../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../repositories/InternetGamePlayerRepository";
import * as Map from "../repositories/MapRepository";
import * as TestDatabase from "./TestDatabase";
import * as Models from "../repositories/Models";
import * as Uuid from "../Uuid";

interface CreateInternetGameConfigurationOptions {
  mapOrMapId?: string | Models.Map;
}

export async function createInternetGameConfiguration(
  options: CreateInternetGameConfigurationOptions
): Promise<Models.InternetGameConfiguration> {
  let mapId: string;
  if (typeof options.mapOrMapId === "string") {
    mapId = options.mapOrMapId;
  } else if (typeof options.mapOrMapId === "undefined") {
    const map = await createMap();
    mapId = map.id;
  } else {
    mapId = options.mapOrMapId.id;
  }
  const newInternetGameConfiguration = Builders.internetGameConfiguration({
    mapId: mapId
  });
  const id = await InternetGameConfigurationRepository.create(
    TestDatabase.query,
    newInternetGameConfiguration
  );
  return await InternetGameConfigurationRepository.findById(TestDatabase.query)(
    id
  );
}

export async function createMap(): Promise<Models.Map> {
  const newMap = Builders.map({});
  return await Map.create(TestDatabase.query, newMap);
}

type InternetGamePlayerOptions = {
  gameId: string;
  playerToken?: string;
};

export async function createInternetGamePlayer(
  options: InternetGamePlayerOptions
): Promise<Models.InternetGamePlayer> {
  return InternetGamePlayerRepository.create(
    TestDatabase.query,
    options.gameId,
    options.playerToken || Uuid.generate()
  );
}
