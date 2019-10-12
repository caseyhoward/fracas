import * as Map from "./Map";
import * as TestDatabase from "./db/TestDatabase";

describe("Map", () => {
  describe(".create", () => {
    it("creates a map", async () => {
      const map = await Map.create(TestDatabase.query, {
        name: "test 1",
        mapJson: '{"abc": 123}'
      });
      const foundMap = await Map.findById(TestDatabase.query, map.id);
      expect(foundMap).toBeDefined();
    });
  });
});
