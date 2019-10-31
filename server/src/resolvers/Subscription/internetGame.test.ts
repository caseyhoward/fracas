import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as InternetGame from "./internetGame";
import * as Graphql from "../../api/graphql";
import * as Builders from "../../test/Builders";
import * as Models from "../../repositories/Models";
import * as GraphqlYoga from "graphql-yoga";

describe("Subscription.internetGame", () => {
  describe(".subscribe", () => {
    it("returns internet game", () => {
      const { pubSub } = mocks();
      const iterator = InternetGame.buildSubscribe(pubSub)(
        {},
        { playerToken: "some-token" },
        {},
        <any>{}
      );
    });
  });

  describe(".resolve", () => {
    it("returns internet game", async () => {
      const playerToken = "asdfasdf";
      const { findGameById, findPlayerByToken } = mocks();
      const expectedGame: Graphql.Game = {
        __typename: "Game",
        id: "2",
        map: <any>{
          id: "1"
        },
        neutralCountryTroops: [],
        playerTurn: {
          __typename: "PlayerTurn",
          playerId: "1",
          playerTurnStage: Models.PlayerTurnStage.CapitolPlacement
        },
        players: [],
        currentUserPlayerId: "1"
      };

      const result = await InternetGame.buildResolve(
        findPlayerByToken,
        findGameById
      )(null, { playerToken: playerToken }, null, <any>{});
      expect(result).toEqual(expectedGame);
    });
  });
});

type Mocks = {
  findGameById: InternetGameRepository.FindById;
  findPlayerByToken: InternetGamePlayerRepository.FindByToken;
  pubSub: GraphqlYoga.PubSub;
};

function mocks(): Mocks {
  const findPlayerByToken: InternetGamePlayerRepository.FindByToken = (
    token: string
  ) => {
    const player = Builders.internetGamePlayer("1", "2");
    return Promise.resolve({ ...player, playerToken: token });
  };
  const findGameById: InternetGameRepository.FindById = (id: string) => {
    const game = Builders.internetGame(id);
    return Promise.resolve(game);
  };
  const pubSub = new GraphqlYoga.PubSub();
  return { findPlayerByToken, findGameById, pubSub };
}
