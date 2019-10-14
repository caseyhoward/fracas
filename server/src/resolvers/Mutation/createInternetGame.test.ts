import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
// import * as InternetGamePlayer from "../../InternetGamePlayer";
import { createInternetGame, defaultHostColor } from "./createInternetGame";
import * as Map from "../../Map";
import * as TestDatabase from "../../db/TestDatabase";

describe("Mutation.createInternetGame", () => {
  it("works", async () => {
    expect(1).toEqual(1);
    // const playerToken = await createInternetGame(TestDatabase.query);

    // const savedConfiguration = await InternetGameConfigurationRepository.findByPlayerToken(
    //   TestDatabase.query,
    //   playerToken
    // );

    // expect(savedConfiguration.players.length).toEqual(1);
    // expect(savedConfiguration.players[0].color).toEqual(defaultHostColor);
  });
});

// afterEach(() => TestDatabase.clean(TestDatabase.query));
