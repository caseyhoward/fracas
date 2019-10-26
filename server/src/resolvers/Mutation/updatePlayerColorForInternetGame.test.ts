import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import updatePlayerColorForInternetGame from "./updatePlayerColorForInternetGame";
import * as TestDatabase from "../../test/TestDatabase";
import * as Color from "../../models/Color";
import { PubSub } from "graphql-yoga";

describe("Mutation.updatePlayerColorForInternetGame", () => {
  it("works", async () => {
    const internetGameConfiguration = await Factories.createInternetGameConfiguration(
      {}
    );
    const internetGamePlayer = await Factories.createInternetGamePlayer({
      gameId: internetGameConfiguration.id
    });
    const configuration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(internetGameConfiguration.id);
    const updatedConfiguration = {
      ...configuration,
      players: [
        { ...Builders.playerConfiguration, playerIdd: "1", color: Color.red },
        {
          ...Builders.playerConfiguration,
          playerIdd: internetGamePlayer.id,
          color: Color.green
        },
        { ...Builders.playerConfiguration, playerIdd: "3", color: Color.blue }
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
