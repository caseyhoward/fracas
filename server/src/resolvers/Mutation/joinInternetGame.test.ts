import * as Factories from "../../test/Factories";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as Player from "../../models/Player";
import * as Color from "../../models/Color";
import { createInternetGame } from "./createInternetGame";
import joinInternetGame from "./joinInternetGame";
import * as TestDatabase from "../../test/TestDatabase";

describe("Mutation.joinInternetGame", () => {
  it("works", async () => {
    await Factories.createMap();
    const hostToken = await createInternetGame(TestDatabase.query); // TODO: Use repositories/factories instead of resolver

    const host = await InternetGamePlayerRepository.findByToken(
      TestDatabase.query
    )(hostToken);

    const configuration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      host.gameId
    );

    const playerToken = await joinInternetGame(TestDatabase.query, {
      joinGameToken: configuration.joinToken
    });

    const configurationWithNewPlayer = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      host.gameId
    );

    const gamePlayer = await InternetGamePlayerRepository.findByToken(
      TestDatabase.query
    )(playerToken);

    expect(gamePlayer.playerToken).toEqual(playerToken);
    expect(configurationWithNewPlayer.players.length).toEqual(2);
    expect(configurationWithNewPlayer.players[0]).toEqual(
      Player.createHost(host.id)
    );
    expect(configurationWithNewPlayer.players[1].color).toEqual(
      Color.lightYellow
    );
  });
});
