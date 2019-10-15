import * as TestDatabase from "../../test/TestDatabase";
import createMap from "./createMap";
import * as MapRepository from "../../repositories/MapRepository";
import * as Fixtures from "../../test/Fixtures";

describe("Mutation.createMap", () => {
  it("gets a map", async () => {
    const map = await createMap(TestDatabase.query, { map: Fixtures.map({}) });
    const resultMap = await MapRepository.findById(
      TestDatabase.query,
      parseInt(map.id, 10)
    );
    expect(MapRepository.mapToGraphql(resultMap)).toEqual(map);
  });
});
