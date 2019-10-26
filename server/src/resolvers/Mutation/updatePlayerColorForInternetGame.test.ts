import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import updatePlayerColorForInternetGame from "./updatePlayerColorForInternetGame";
import * as TestDatabase from "../../test/TestDatabase";
import * as Color from "../../models/Color";
import * as Models from "../../repositories/Models";
import { PubSub } from "graphql-yoga";

describe("Mutation.updatePlayerColorForInternetGame", () => {
  it("works", async () => {
    const internetGameConfiguration = await Factories.createInternetGameConfiguration(
      {}
    );
    const internetGamePlayer = await Factories.createInternetGamePlayer({
      gameId: internetGameConfiguration.id
    });
    const configuration: Models.InternetGameConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(internetGameConfiguration.id);
    const updatedConfiguration: Models.InternetGameConfiguration = {
      ...configuration,
      players: [
        { ...Builders.playerConfiguration, playerId: "1", color: Color.red },
        {
          ...Builders.playerConfiguration,
          playerId: internetGamePlayer.id,
          color: Color.green
        },
        { ...Builders.playerConfiguration, playerId: "3", color: Color.blue }
      ]
    };
    await InternetGameConfigurationRepository.save(
      TestDatabase.query,
      updatedConfiguration
    );
    await updatePlayerColorForInternetGame(TestDatabase.query, new PubSub(), {
      playerToken: internetGamePlayer.playerToken,
      color: Color.black
    });
    const retrievedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(internetGameConfiguration.id);
    expect(retrievedConfiguration.players[1].color).toEqual(Color.black);
  });
});
