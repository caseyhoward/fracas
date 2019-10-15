import * as Fixtures from "./Fixtures";
import * as InternetGameConfigurationRepository from "../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../repositories/InternetGamePlayerRepository";
import * as Map from "../repositories/MapRepository";
import * as TestDatabase from "../test/TestDatabase";
import * as Models from "../repositories/Models";
import * as Uuid from "../Uuid";

interface CreateInternetGameConfigurationOptions {
  mapOrMapId?: number | Models.Map;
}

export async function createInternetGameConfiguration(
  options: CreateInternetGameConfigurationOptions
): Promise<Models.InternetGameConfiguration> {
  let mapId: number;
  if (typeof options.mapOrMapId === "number") {
    mapId = options.mapOrMapId;
  } else if (typeof options.mapOrMapId === "undefined") {
    const map = await createMap();
    mapId = map.id;
  } else {
    mapId = options.mapOrMapId.id;
  }
  const newInternetGameConfiguration = Fixtures.internetGameConfiguration({
    mapId: mapId
  });
  const id = await InternetGameConfigurationRepository.create(
    TestDatabase.query,
    newInternetGameConfiguration
  );
  return await InternetGameConfigurationRepository.findById(
    TestDatabase.query,
    id
  );
}

export async function createMap(): Promise<Models.Map> {
  const newMap = Fixtures.map({});
  return await Map.create(TestDatabase.query, newMap);
}

type InternetGamePlayerOptions = {
  gameId: number;
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
