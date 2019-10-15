import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import startInternetGame from "./startInternetGame";
import * as TestDatabase from "../../test/TestDatabase";

describe("Mutation.startInternetGame", () => {
  it("starts a game", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});
    const host = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });
    await InternetGameConfigurationRepository.addPlayer(
      TestDatabase.query,
      host.id,
      Builders.player({ id: host.id })
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
});
