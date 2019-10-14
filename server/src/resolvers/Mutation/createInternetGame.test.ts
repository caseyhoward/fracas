import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import { createInternetGame, defaultHostColor } from "./createInternetGame";
import * as Map from "../../repositories/MapRepository";
import * as TestDatabase from "../../db/TestDatabase";

describe("Mutation.createInternetGame", () => {
  it("works", async () => {
    await Map.create(TestDatabase.query, {
      name: "blah",
      countries: [],
      bodiesOfWater: [],
      dimensions: { width: 0, height: 0 }
    });
    const hostToken = await createInternetGame(TestDatabase.query);

    const player = await InternetGamePlayerRepository.findByToken(
      TestDatabase.query,
      hostToken
    );

    const configuration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      player.gameId
    );

    expect(configuration.players.length).toEqual(1);
    expect(configuration.players[0].color).toEqual(defaultHostColor);
  });
});
