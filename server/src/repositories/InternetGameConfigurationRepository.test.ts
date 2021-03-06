import * as InternetGameConfigurationRepository from "./InternetGameConfigurationRepository";
import * as TestDatabase from "../test/TestDatabase";

import * as Models from "./Models";
import * as Uuid from "../Uuid";
import * as Player from "../models/Player";
import * as Builders from "../test/Builders";

describe("InternetGameConfigurationRepository.addPlayer", () => {
  it("adds a new player", async () => {
    const newConfiguration: Models.NewInternetGameConfiguration = {
      __typename: "NewInternetGameConfiguration",
      players: [],
      mapId: Models.userMapId("1"),
      joinToken: "blah"
    };
    const configurationId = await InternetGameConfigurationRepository.create(
      TestDatabase.query,
      newConfiguration
    );
    const player: Player.PlayerConfiguration = {
      ...Builders.playerConfiguration,
      playerId: "123",
      name: "test name"
    };
    await InternetGameConfigurationRepository.addPlayer(
      TestDatabase.query,
      configurationId,
      player
    );
    const savedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(configurationId);
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
      mapId: Models.userMapId("1"),
      joinToken: joinToken
    };
    const configurationId = await InternetGameConfigurationRepository.create(
      TestDatabase.query,
      newConfiguration
    );
    const savedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(configurationId);
    expect(savedConfiguration.mapId.value).toEqual("1");
  });
});

describe("InternetGameConfigurationRepository.findByJoinToken", () => {
  it("creates a new Internet game", async () => {
    const joinToken = Uuid.generate();
    const newConfiguration: Models.NewInternetGameConfiguration = {
      __typename: "NewInternetGameConfiguration",
      players: [],
      mapId: Models.userMapId("1"),
      joinToken: joinToken
    };
    const configurationId = await InternetGameConfigurationRepository.create(
      TestDatabase.query,
      newConfiguration
    );
    const savedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(configurationId);
    const savedConfigurationByJoinToken = await InternetGameConfigurationRepository.findByJoinToken(
      TestDatabase.query,
      savedConfiguration.joinToken
    );
    expect(savedConfigurationByJoinToken).toEqual(savedConfiguration);
  });
});
