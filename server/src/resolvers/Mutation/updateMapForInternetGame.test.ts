import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import { updateMapForInternetGame } from "./updateMapForInternetGame";
import * as TestDatabase from "../../test/TestDatabase";
import * as Color from "../../models/Color";
import { PubSub } from "graphql-yoga";

describe("Mutation.updateMapForInternetGame", () => {
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
        { ...Builders.playerConfiguration, color: Color.red },
        {
          ...Builders.playerConfiguration,
          id: internetGamePlayer.id,
          color: Color.red
        },
        { ...Builders.playerConfiguration, color: Color.red }
      ]
    };
    await InternetGameConfigurationRepository.save(
      TestDatabase.query,
      updatedConfiguration
    );
    // await updateMapForInternetGame(TestDatabase.query, new PubSub(), {
    //   playerToken: internetGamePlayer.playerToken,
    //   color: Color.black
    // });
    // const retrievedConfiguration = await InternetGameConfigurationRepository.findById(
    //   TestDatabase.query
    // )(internetGameConfiguration.id);
    // expect(retrievedConfiguration.players[1].color).toEqual(Color.black);
  });
});
