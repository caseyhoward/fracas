import * as TestDatabase from "../../test/TestDatabase";
import queryMap from "./map";
import * as MapRepository from "../../repositories/MapRepository";
import * as Factories from "../../test/Factories";
import * as Models from "../../repositories/Models";

describe("Query.map", () => {
  it("gets a map", async () => {
    const map: Models.Map = await Factories.createMap();
    const resultMap = await queryMap(TestDatabase.query, {
      id: map.id.value
    });
    expect(resultMap).toEqual(MapRepository.mapToGraphql(map));
  });
});
