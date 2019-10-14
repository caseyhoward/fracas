import * as InternetGameRepository from "./InternetGameRepository";
import * as InternetGamePlayer from "../InternetGamePlayer";
import * as Map from "../Map";
import * as TestDatabase from "../db/TestDatabase";

describe("InternetGame", () => {
  describe(".create", () => {
    it("works", async () => {
      const newConfiguration: InternetGameRepository.NewConfiguration = {
        __typename: "NewConfiguration",
        players: [],
        mapId: 1,
        joinToken: "blah"
      };
      const configurationId = await InternetGameRepository.create(
        TestDatabase.query,
        newConfiguration
      );
      const savedConfiguration = await InternetGameRepository.findById(
        TestDatabase.query,
        configurationId
      );
      expect(savedConfiguration.mapId).toEqual(1);
    });
  });
});

// await Map.create(TestDatabase.query, {
//   name: "abc",
//   countries: [],
//   bodiesOfWater: [],
//   dimensions: { width: 0, height: 0 }
// });
// const playerToken = await InternetGame.create(TestDatabase.query);
// const player = await InternetGamePlayer.findByToken(
//   TestDatabase.query,
//   playerToken
// );
// const game = await InternetGame.findById(
//   TestDatabase.query,
//   player.gameId
// );
