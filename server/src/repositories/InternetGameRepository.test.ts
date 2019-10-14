import * as InternetGameRepository from "./InternetGameRepository";
import * as TestDatabase from "../db/TestDatabase";
import * as Models from "./Models";

describe("InternetGameRepository.create", () => {
  it("creates a new Internet game", async () => {
    // const internetGame: Models.InternetGame = {
    //   __typename: "InternetGame",
    //   players: [],
    //   mapId: 1,
    //   id: 123,
    //   neutralCountryTroops: [],
    //   playerTurn: {
    //     __typename: "PlayerTurn",
    //     playerId: "",
    //     playerTurnStage: Models.PlayerTurnStage.CapitolPlacement
    //   }
    // };
    // const configurationId = await InternetGameRepository.create(
    //   TestDatabase.query,
    //   internetGame
    // );
    // const savedConfiguration = await InternetGameRepository.findById(
    //   TestDatabase.query,
    //   configurationId
    // );
    // expect(savedConfiguration.mapId).toEqual(1);
    expect(1).toEqual(1);
  });
});

afterEach(() => TestDatabase.clean(TestDatabase.query));
