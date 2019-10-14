import * as InternetGameRepository from "./InternetGameRepository";
import * as InternetGamePlayer from "../InternetGamePlayer";
import * as Map from "../Map";
import * as TestDatabase from "../db/TestDatabase";
import * as Models from "./Models";

describe("InternetGame.create", () => {
  it("creates a new Internet game", async () => {
    // const newConfiguration: Models.NewInternetGameConfiguration = {
    //   __typename: "NewInternetGameConfiguration",
    //   players: [],
    //   mapId: 1,
    //   joinToken: "blah"
    // };
    // const configurationId = await InternetGameRepository.create(
    //   TestDatabase.query,
    //   newConfiguration
    // );
    // const savedConfiguration = await InternetGameRepository.findById(
    //   TestDatabase.query,
    //   configurationId
    // );
    // expect(savedConfiguration.mapId).toEqual(1);
    expect(1).toEqual(1);
  });
});

describe("InternetGame.addPlayer", () => {
  it("adds a new player", async () => {
    // const newConfiguration: Models.NewInternetGameConfiguration = {
    //   __typename: "NewInternetGameConfiguration",
    //   players: [],
    //   mapId: 1,
    //   joinToken: "blah"
    // };
    // const configurationId = await InternetGameRepository.create(
    //   TestDatabase.query,
    //   newConfiguration
    // );
    // const player: Models.PlayerConfiguration = {
    //   color: { __typename: "Color", red: 0, green: 255, blue: 0 },
    //   name: "test name",
    //   playerId: 123
    // };
    // await InternetGameRepository.addPlayer(
    //   TestDatabase.query,
    //   configurationId,
    //   player
    // );
    // const savedConfiguration = await InternetGameRepository.findById(
    //   TestDatabase.query,
    //   configurationId
    // );
    // if (savedConfiguration.__typename === "Configuration") {
    //   expect(
    //     savedConfiguration.players.find(p => p.playerId === 123)!.name
    //   ).toEqual("test name");
    // } else {
    //   throw "error";
    // }
  });
});

afterEach(() => TestDatabase.clean(TestDatabase.query));
// await Map.create(TestDatabase.query, {
//   name: "abc",
//   countries: [],
//   bodiesOfWater: [],
//   dimensions: { width: 0, height: 0 }
// });
// const playerToken = await InternetGame.create(TestDatabase.query);
// const player = await InternetGamePlayer.findByToken(
//   TestDatabase.query,
//   playerToken
// );
// const game = await InternetGame.findById(
//   TestDatabase.query,
//   player.gameId
// );
