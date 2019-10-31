import * as Factories from "../../test/Factories";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import saveInternetGame from "./saveInternetGame";
import * as Models from "../../repositories/Models";
import * as TestDatabase from "../../test/TestDatabase";
import * as Builders from "../../test/Builders";
import * as PubSub from "../../PubSub";

describe("Mutation.saveInternetGame", () => {
  it("starts a game", async () => {
    const pubSub = PubSub.inMemory();
    const configuration = await Factories.createInternetGameConfiguration({});

    const player = await Factories.createInternetGamePlayer({
      gameId: configuration.id
    });

    const internetGameToSave: Models.InternetGameWithoutMap = Builders.internetGameWithoutMap(
      configuration.id
    );

    await saveInternetGame(TestDatabase.query, pubSub, {
      playerToken: player.playerToken,
      game: internetGameToSave
    });

    const game = await InternetGameRepository.findById(TestDatabase.query)(
      configuration.id
    );

    expect(game.id).toEqual(configuration.id);
  });
});
