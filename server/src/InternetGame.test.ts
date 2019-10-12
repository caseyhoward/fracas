import * as InternetGame from "./InternetGame";
import * as InternetGamePlayer from "./InternetGamePlayer";
import * as Map from "./Map";
import * as TestDatabase from "./db";

describe("InternetGame", () => {
  describe(".create", () => {
    it("works", async () => {
      await Map.create(TestDatabase.query, { name: "abc", mapJson: "" });
      const playerToken = await InternetGame.create(TestDatabase.query);
      const player = await InternetGamePlayer.findByToken(
        TestDatabase.query,
        playerToken
      );
      // const game = await InternetGame.findById(Database.query, player.gameId);
    });
  });
});
