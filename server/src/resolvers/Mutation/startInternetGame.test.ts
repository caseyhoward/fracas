import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import startInternetGame from "./startInternetGame";
import * as TestDatabase from "../../test/TestDatabase";

describe("Mutation.startInternetGame", () => {
  it("starts a game when host", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});
    const host = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });
    await InternetGameConfigurationRepository.addPlayer(
      TestDatabase.query,
      configuration.id,
      Builders.playerConfiguration({ id: host.id })
    );
    await startInternetGame(TestDatabase.query, {
      playerToken: host.playerToken
    });
    const game = await InternetGameRepository.findById(
      TestDatabase.query,
      configuration.id
    );
    expect(game.__typename).toEqual("InternetGame");
  });

  it("doesn't start a game when not host", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});
    const host = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });
    const notHost = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });
    await InternetGameConfigurationRepository.addPlayer(
      TestDatabase.query,
      configuration.id,
      Builders.playerConfiguration({ id: host.id })
    );
    await InternetGameConfigurationRepository.addPlayer(
      TestDatabase.query,
      configuration.id,
      Builders.playerConfiguration({ id: notHost.id })
    );
    await startInternetGame(TestDatabase.query, {
      playerToken: notHost.playerToken
    });
    const game = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      configuration.id
    );
    expect(game.__typename).toEqual("InternetGameConfiguration");
  });
});
