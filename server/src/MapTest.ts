import * as Map from "./Map";
import * as TestDatabase from "./db/TestDatabase";

describe("Map", () => {
  describe(".create", () => {
    it("creates a map", async () => {
      const map = await Map.create(TestDatabase.query, {
        name: "abc",
        countries: [],
        bodiesOfWater: [],
        dimensions: { width: 0, height: 0 }
      });
      const foundMap = await Map.findById(TestDatabase.query, map.id);
      expect(foundMap.name).toEqual("abc");
    });
  });
});
