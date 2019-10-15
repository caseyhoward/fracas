import * as Fixtures from "./Fixtures";
import * as InternetGameConfigurationRepository from "../repositories/InternetGameConfigurationRepository";
import * as Map from "../repositories/MapRepository";
import * as TestDatabase from "../db/TestDatabase";
import * as Models from "../repositories/Models";

export async function createInternetGameConfiguration() {
  const map = await createMap();
  const newInternetGameConfiguration = Fixtures.internetGameConfiguration({
    mapId: map.id
  });
  return InternetGameConfigurationRepository.create(
    TestDatabase.query,
    newInternetGameConfiguration
  );
}

export async function createMap(): Promise<Models.Map> {
  const newMap = Fixtures.map({});
  return await Map.create(TestDatabase.query, newMap);
}
