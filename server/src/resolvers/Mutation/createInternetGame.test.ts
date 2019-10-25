import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import { createInternetGame } from "./createInternetGame";
import * as MapRepository from "../../repositories/MapRepository";
import * as Player from "../../models/Player";
import * as TestDatabase from "../../test/TestDatabase";

describe("Mutation.createInternetGame", () => {
  it("works", async () => {
    await MapRepository.create(TestDatabase.query, {
      name: "blah",
      countries: [],
      bodiesOfWater: [],
      dimensions: { width: 0, height: 0 }
    });
    const hostToken = await createInternetGame(TestDatabase.query);

    const player = await InternetGamePlayerRepository.findByToken(
      TestDatabase.query
    )(hostToken);

    const configuration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(player.gameId);

    expect(configuration.players.length).toEqual(1);
    expect(configuration.players[0]).toEqual(Player.createHost(player.id));
  });
});
