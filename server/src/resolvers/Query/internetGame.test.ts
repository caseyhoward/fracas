import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import internetGame from "./internetGame";
import * as TestDatabase from "../../test/TestDatabase";
import * as Models from "../../repositories/Models";
import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";

describe("Query.internetGame", () => {
  it("returns internet game", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});

    const internetGamePlayer = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });

    const player: Models.Player = {
      __typename: "Player",
      color: { __typename: "Color", red: 0, green: 255, blue: 0 },
      name: "test name",
      id: internetGamePlayer.id,
      countryTroopCounts: [],
      ports: []
    };

    const updatedConfiguration = await InternetGameConfigurationRepository.findById(
      TestDatabase.query,
      configuration.id
    );

    const game: Models.InternetGame = Builders.internetGame(
      updatedConfiguration.id
    );

    const configurationId = await InternetGameRepository.save(
      TestDatabase.query,
      { ...game, players: [player] }
    );

    const gameOrConfiguration = await internetGame(TestDatabase.query, {
      playerToken: internetGamePlayer.playerToken
    });

    expect(gameOrConfiguration.game.players.length).toEqual(1);
    expect(gameOrConfiguration.game.players[0].name).toEqual("test name");
  });
});
