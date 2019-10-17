import * as Database from "../../Database";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

import * as Uuid from "../../Uuid";
import * as Models from "../../repositories/Models";
import * as graphql from "../../api/graphql";
import * as Player from "../../models/Player";

export default async function joinInternetGame(
  executeQuery: Database.ExecuteQuery,
  input: graphql.RequireFields<
    graphql.MutationJoinInternetGameArgs,
    "joinGameToken"
  >
): Promise<string> {
  const playerToken = Uuid.generate();
  const internetGame = await InternetGameConfigurationRepository.findByJoinToken(
    executeQuery,
    input.joinGameToken
  );
  const newPlayer = await InternetGamePlayerRepository.create(
    executeQuery,
    internetGame.id,
    playerToken
  );
  const updatedPlayers: Models.PlayerConfiguration[] = [
    ...internetGame.players,
    {
      __typename: "PlayerConfiguration",
      playerId: newPlayer.id,
      name: "",
      color: getNextAvailablePlayerColor(internetGame.players)
    }
  ];
  const updatedGame: Models.InternetGameConfiguration = {
    ...internetGame,
    players: updatedPlayers
  };
  await InternetGameConfigurationRepository.save(executeQuery, updatedGame);

  return playerToken;
}

function getNextAvailablePlayerColor(
  players: Models.PlayerConfiguration[]
): Models.Color {
  return Player.allowedColors[0];
}
