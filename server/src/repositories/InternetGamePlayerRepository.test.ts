import * as Models from "./Models";
import * as InternetGamePlayerRepository from "./InternetGamePlayerRepository";
import * as TestDatabase from "../db/TestDatabase";

describe("InternetGameConfigurationRepository.create and .findByToken", () => {
  it("creates a new player", async () => {
    const player = await InternetGamePlayerRepository.create(
      TestDatabase.query,
      123,
      "some token"
    );
    const foundPlayer = await InternetGamePlayerRepository.findByToken(
      TestDatabase.query,
      "some token"
    );
    expect(foundPlayer.gameId).toEqual(123);
    expect(foundPlayer.playerToken).toEqual("some token");
  });
});

// afterEach(() => TestDatabase.clean(TestDatabase.query));
