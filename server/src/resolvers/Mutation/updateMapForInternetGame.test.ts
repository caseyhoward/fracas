import * as Factories from "../../test/Factories";
import * as Builders from "../../test/Builders";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import { updateMapForInternetGame } from "./updateMapForInternetGame";
import * as TestDatabase from "../../test/TestDatabase";
import * as Color from "../../models/Color";
import * as GraphqlYoga from "graphql-yoga";
import * as PubSub from "../../PubSub";

import * as Models from "../../repositories/Models";

describe("Mutation.updateMapForInternetGame", () => {
  it("works", async () => {
    const playerToken = "some token";
    const gameId = "some game id";
    const mapId = "some map id";
    const playerId = "some player id";
    const internetGamePlayer: Models.InternetGamePlayer = {
      ...Builders.internetGamePlayer(playerId, gameId)
    };
    const pubSub = new GraphqlYoga.PubSub();

    pubSub.subscribe("", () => null);
    const findInternetGamePlayerByToken: InternetGamePlayerRepository.FindByToken = async actualPlayerToken => {
      expect(playerToken).toEqual(actualPlayerToken);
      return Promise.resolve(internetGamePlayer);
    };
    const updateMap: InternetGameConfigurationRepository.UpdateMap = async (
      actualGameId,
      actualMapId
    ) => {
      expect(actualGameId).toEqual(gameId);
      expect(actualMapId).toEqual(mapId);
      return Promise.resolve();
    };

    const result = await updateMapForInternetGame(
      findInternetGamePlayerByToken,
      updateMap,
      pubSub,
      { mapId, playerToken }
    )();

    expect(result).toEqual(true);
  });
});
