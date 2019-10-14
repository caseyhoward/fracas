import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import internetGame from "./internetGame";
import * as TestDatabase from "../../db/TestDatabase";
import * as Models from "../../repositories/Models";
import * as Uuid from "../../Uuid";
import { defaultHostColor } from "../Mutation/createInternetGame";

describe.only("Query.internetGame", () => {
  describe("with configuration", () => {
    it("returns configuration", async () => {
      const joinToken = Uuid.generate();
      const playerToken = Uuid.generate();

      const newConfiguration: Models.NewInternetGameConfiguration = {
        __typename: "NewInternetGameConfiguration",
        players: [],
        mapId: 1,
        joinToken
      };

      const configurationId = await InternetGameConfigurationRepository.create(
        TestDatabase.query,
        newConfiguration
      );

      const player: Models.PlayerConfiguration = {
        __typename: "PlayerConfiguration",
        color: { __typename: "Color", red: 0, green: 255, blue: 0 },
        name: "test name",
        playerId: 123
      };

      InternetGamePlayerRepository.create(
        TestDatabase.query,
        configurationId,
        playerToken
      );

      await InternetGameConfigurationRepository.addPlayer(
        TestDatabase.query,
        configurationId,
        player
      );

      const gameOrConfiguration = await internetGame(TestDatabase.query, {
        playerToken
      });

      if (gameOrConfiguration.__typename === "InternetGameConfiguration") {
        expect(gameOrConfiguration.players.length).toEqual(1);
        expect(gameOrConfiguration.players[0].color).toEqual(defaultHostColor);
      }
    });
  });
});

// afterEach(() => TestDatabase.clean(TestDatabase.query));
