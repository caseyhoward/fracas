import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import internetGameOrConfiguration from "./internetGameOrConfiguration";
import * as TestDatabase from "../../test/TestDatabase";
import * as Models from "../../repositories/Models";
import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";
import * as Player from "../../models/Player";
import * as Color from "../../models/Color";

describe("Query.internetGameOrConfiguration", () => {
  it("returns configuration", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});

    const internetGamePlayer = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });
    const player: Player.PlayerConfiguration = Player.createHost(
      internetGamePlayer.id
    );

    await InternetGameConfigurationRepository.addPlayer(
      TestDatabase.query,
      configuration.id,
      player
    );

    const gameOrConfiguration = await internetGameOrConfiguration(
      TestDatabase.query,
      {
        playerToken: internetGamePlayer.playerToken
      }
    );

    if (gameOrConfiguration.__typename === "InternetGameConfiguration") {
      expect(gameOrConfiguration.players.length).toEqual(1);
      expect(gameOrConfiguration.players[0].color).toEqual(Color.lightGreen);
    } else {
      fail("Query.internetGameOrConfiguration: Must be Configuration");
    }
  });

  it("returns internet game", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});

    const internetGamePlayer = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });

    const player: Models.Player = {
      __typename: "Player",
      color: Color.lightGreen,
      name: "test name",
      id: internetGamePlayer.id,
      countryTroopCounts: [],
      ports: []
    };

    const updatedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query
    )(configuration.id);

    const game: Models.InternetGame = Builders.internetGame(
      updatedConfiguration.id
    );

    const configurationId = await InternetGameRepository.save(
      TestDatabase.query,
      { ...game, players: [player] }
    );

    const gameOrConfiguration = await internetGameOrConfiguration(
      TestDatabase.query,
      {
        playerToken: internetGamePlayer.playerToken
      }
    );

    if (gameOrConfiguration.__typename === "Game") {
      expect(gameOrConfiguration.players.length).toEqual(1);
      expect(gameOrConfiguration.players[0].name).toEqual("test name");
    } else {
      fail("Must be InternetGame");
    }
  });
});
