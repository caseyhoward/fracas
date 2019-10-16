import * as Database from "../../Database";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGameRepository from "../../repositories/InternetGameRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

import * as Models from "../../repositories/Models";
import * as graphql from "../../api/graphql";

export default async function saveInternetGame(
  executeQuery: Database.ExecuteQuery,
  input: graphql.RequireFields<
    graphql.MutationSaveInternetGameArgs,
    "playerToken" | "game"
  >
): Promise<boolean> {
  const player = await InternetGamePlayerRepository.findByToken(
    executeQuery,
    input.playerToken
  );

  // await InternetGameRepository.save(executeQuery, {
  //   ...input.game,
  //   __typename: "InternetGame",
  //   id: parseInt(input.game.id, 10),
  //   mapId: parseInt(input.game.mapId, 10),
  //   players: input.game.players.map(player => {
  //     return {
  //       ...player,
  //       id: parseInt(player.id, 10),
  //       __typename: "Player",
  //       countryTroopCounts: player.countryTroopCounts.map(
  //         countryTroopCounts => {
  //           return { ...countryTroopCounts, __typename: "CountryTroopCounts" };
  //         }
  //       )
  //     };
  //   })
  // });

  return true;
}

function generateRandomTroopCounts(): Models.CountryTroopCounts[] {
  return [];
}

function playerConfigurationToPlayer(
  playerConfiguraation: Models.PlayerConfiguration
): Models.Player {
  return {
    __typename: "Player",
    id: playerConfiguraation.playerId,
    name: playerConfiguraation.name,
    countryTroopCounts: [],
    color: playerConfiguraation.color,
    ports: []
  };
}
