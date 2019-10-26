import * as Database from "../../Database";
import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";
import * as PubSub from "../../PubSub";
import * as Uuid from "../../Uuid";
import * as Models from "../../repositories/Models";
import * as graphql from "../../api/graphql";
import * as Player from "../../models/Player";

export default async function joinInternetGame(
  executeQuery: Database.ExecuteQuery,
  pubSub: PubSub.PubSub,
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
  const updatedPlayers: Player.PlayerConfiguration[] = [
    ...internetGame.players,
    {
      __typename: "PlayerConfiguration",
      playerId: newPlayer.id,
      name: "",
      color: Player.getNextAvailablePlayerColor(internetGame.players)
    }
  ];
  const updatedGame: Models.InternetGameConfiguration = {
    ...internetGame,
    players: updatedPlayers
  };
  await InternetGameConfigurationRepository.save(executeQuery, updatedGame);
  PubSub.internetGameConfigurationChanged(pubSub, updatedGame);
  return playerToken;
}
