import * as Factories from "../../test/Factories";
import * as Fixtures from "../../test/Fixtures";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import updatePlayerColorForInternetGame from "./updatePlayerColorForInternetGame";
import * as TestDatabase from "../../test/TestDatabase";
import * as Models from "../../repositories/Models";

describe("Mutation.updatePlayerColorForInternetGame", () => {
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
        Fixtures.player({ id: 1, color: { ...black, red: 0 } }),
        Fixtures.player({
          id: internetGamePlayer.id,
          color: { ...black, green: 0 }
        }),
        Fixtures.player({ id: 3, color: { ...black, blue: 0 } })
      ]
    };
    await InternetGameConfigurationRepository.save(
      TestDatabase.query,
      updatedConfiguration
    );
    await updatePlayerColorForInternetGame(TestDatabase.query, {
      playerToken: internetGamePlayer.playerToken,
      color: black
    });
    const retrievedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      internetGameConfiguration.id
    );
    expect(retrievedConfiguration.players[1].color).toEqual(black);
  });
});

const black: Models.Color = { __typename: "Color", red: 0, green: 0, blue: 0 };