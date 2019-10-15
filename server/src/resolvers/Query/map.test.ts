import * as TestDatabase from "../../test/TestDatabase";
import queryMap from "./map";
import * as MapRepository from "../../repositories/MapRepository";
import * as Factories from "../../test/Factories";

describe("Query.map", () => {
  it("gets a map", async () => {
    const map = await Factories.createMap();
    const resultMap = await queryMap(TestDatabase.query, {
      id: map.id.toString()
    });
    expect(resultMap).toEqual(MapRepository.mapToGraphql(map));
  });
});
