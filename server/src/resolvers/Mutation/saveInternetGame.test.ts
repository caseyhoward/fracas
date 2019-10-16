import * as Factories from "../../test/Factories";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import saveInternetGame from "./saveInternetGame";
import * as Models from "../../repositories/Models";
import * as TestDatabase from "../../test/TestDatabase";

describe("Mutation.saveInternetGame", () => {
  it("starts a game", async () => {
    const configuration = await Factories.createInternetGameConfiguration({});

    const player = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });

    const internetGameToSave: Models.InternetGame = {
      __typename: "InternetGame",
      id: configuration.id,
      mapId: "1234",
      players: [],
      neutralCountryTroops: [],
      playerTurn: {
        __typename: "PlayerTurn",
        playerId: "1",
        playerTurnStage: Models.PlayerTurnStage.CapitolPlacement
      }
    };

    await saveInternetGame(TestDatabase.query, {
      playerToken: player.playerToken,
      game: internetGameToSave
    });

    const game = await InternetGameRepository.findById(
      TestDatabase.query,
      configuration.id
    );

    expect(game.mapId).toEqual("1234");
  });
});
