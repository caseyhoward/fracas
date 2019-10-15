import * as Factories from "../../test/Factories";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import { createInternetGame, defaultHostColor } from "./createInternetGame";
import joinInternetGame from "./joinInternetGame";
import * as Map from "../../repositories/MapRepository";
import * as TestDatabase from "../../db/TestDatabase";

describe("Mutation.joinInternetGame", () => {
  it("works", async () => {
    await Factories.createMap();
    const hostToken = await createInternetGame(TestDatabase.query);

    const player = await InternetGamePlayerRepository.findByToken(
      TestDatabase.query,
      hostToken
    );

    const configuration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      player.gameId
    );

    const playerToken = await joinInternetGame(TestDatabase.query, {
      joinGameToken: configuration.joinToken
    });

    const configurationWithNewPlayer = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      player.gameId
    );

    const gamePlayer = await InternetGamePlayerRepository.findByToken(
      TestDatabase.query,
      playerToken
    );

    expect(gamePlayer.playerToken).toEqual(playerToken);
    expect(configurationWithNewPlayer.players.length).toEqual(2);
    expect(configurationWithNewPlayer.players[0].color).toEqual(
      defaultHostColor
    );
  });
});
