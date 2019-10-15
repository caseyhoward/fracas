import * as TestDatabase from "../../test/TestDatabase";
import maps from "./maps";
import * as Factories from "../../test/Factories";
import * as MapRepository from "../../repositories/MapRepository";

describe("Query.map", () => {
  it("gets a map", async () => {
    const map = await Factories.createMap();
    const allMaps = await maps(TestDatabase.query);
    expect(allMaps).toContainEqual(MapRepository.mapToGraphql(map));
  });
});
