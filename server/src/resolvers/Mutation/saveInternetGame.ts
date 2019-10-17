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

  const playerTurn: Models.PlayerTurn = {
    ...input.game.playerTurn,
    __typename: "PlayerTurn",
    fromCountryId: input.game.playerTurn.fromCountryId || undefined,
    troopCount: input.game.playerTurn.troopCount || undefined
  };
  const internetGame: Models.InternetGame = {
    ...input.game,
    __typename: "InternetGame",
    id: input.game.id,
    mapId: input.game.mapId,
    playerTurn: playerTurn,
    neutralCountryTroops: input.game.neutralCountryTroops.map(
      neutralCountryTroops => {
        return { ...neutralCountryTroops, __typename: "CountryTroopCounts" };
      }
    ),
    players: input.game.players.map(player => {
      return {
        ...player,
        id: player.id,
        color: { ...player.color, __typename: "Color" },
        __typename: "Player",
        countryTroopCounts: player.countryTroopCounts.map(
          countryTroopCounts => {
            return { ...countryTroopCounts, __typename: "CountryTroopCounts" };
          }
        ),
        capitol: player.capitol || undefined
      };
    })
  };

  await InternetGameRepository.save(executeQuery, internetGame);

  return true;
}

function generateRandomTroopCounts(): Models.CountryTroopCounts[] {
  return [];
}
