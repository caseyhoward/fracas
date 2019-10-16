import * as InternetGameConfigurationRepository from "./InternetGameConfigurationRepository";
import * as TestDatabase from "../test/TestDatabase";

import * as Models from "./Models";
import * as Uuid from "../Uuid";

describe("InternetGameConfigurationRepository.addPlayer", () => {
  it("adds a new player", async () => {
    const newConfiguration: Models.NewInternetGameConfiguration = {
      __typename: "NewInternetGameConfiguration",
      players: [],
      mapId: "1",
      joinToken: "blah"
    };
    const configurationId = await InternetGameConfigurationRepository.create(
      TestDatabase.query,
      newConfiguration
    );
    const player: Models.PlayerConfiguration = {
      __typename: "PlayerConfiguration",
      color: { __typename: "Color", red: 0, green: 255, blue: 0 },
      name: "test name",
      playerId: "123"
    };
    await InternetGameConfigurationRepository.addPlayer(
      TestDatabase.query,
      configurationId,
      player
    );
    const savedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      configurationId
    );
    expect(
      savedConfiguration.players.find(p => p.playerId === "123")!.name
    ).toEqual("test name");
  });
});

describe("InternetGameConfigurationRepository.create", () => {
  it("creates a new Internet game", async () => {
    const joinToken = Uuid.generate();
    const newConfiguration: Models.NewInternetGameConfiguration = {
      __typename: "NewInternetGameConfiguration",
      players: [],
      mapId: "1",
      joinToken: joinToken
    };
    const configurationId = await InternetGameConfigurationRepository.create(
      TestDatabase.query,
      newConfiguration
    );
    const savedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      configurationId
    );
    expect(savedConfiguration.mapId).toEqual(1);
  });
});

describe("InternetGameConfigurationRepository.findByJoinToken", () => {
  it("creates a new Internet game", async () => {
    const joinToken = Uuid.generate();
    const newConfiguration: Models.NewInternetGameConfiguration = {
      __typename: "NewInternetGameConfiguration",
      players: [],
      mapId: "1",
      joinToken: joinToken
    };
    const configurationId = await InternetGameConfigurationRepository.create(
      TestDatabase.query,
      newConfiguration
    );
    const savedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      configurationId
    );
    const savedConfigurationByJoinToken = await InternetGameConfigurationRepository.findByJoinToken(
      TestDatabase.query,
      savedConfiguration.joinToken
    );
    expect(savedConfigurationByJoinToken).toEqual(savedConfiguration);
  });
});
