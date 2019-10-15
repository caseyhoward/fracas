import * as Factories from "../../test/Factories";
import * as Fixtures from "../../test/Fixtures";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import updatePlayerNameForInternetGame from "./updatePlayerNameForInternetGame";
import * as TestDatabase from "../../db/TestDatabase";

describe("Mutation.updatePlayerNameForInternetGame", () => {
  it("works", async () => {
    const internetGameConfiguration = await Factories.createInternetGameConfiguration(
      {}
    );
    const internetGamePlayer = await Factories.createInternetGamePlayer({
      gameId: internetGameConfiguration.id
    });
    const configuration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      internetGameConfiguration.id
    );
    const updatedConfiguration = {
      ...configuration,
      players: [
        Fixtures.player({ id: 1, name: "some name 1" }),
        Fixtures.player({ id: internetGamePlayer.id, name: "some name 2" }),
        Fixtures.player({ id: 3, name: "some name 3" })
      ]
    };
    await InternetGameConfigurationRepository.save(
      TestDatabase.query,
      updatedConfiguration
    );
    await updatePlayerNameForInternetGame(TestDatabase.query, {
      playerToken: internetGamePlayer.playerToken,
      name: "new name"
    });
    const retrievedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      internetGameConfiguration.id
    );
    expect(retrievedConfiguration.players[1].name).toEqual("new name");
  });
});
