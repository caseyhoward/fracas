import * as Database from "../../Database";
// import * as InternetGameConfigurationRepository from "../../repositories/InternetGameConfigurationRepository";
// import * as InternetGamePlayerRepository from "../../repositories/InternetGamePlayerRepository";

// import * as Uuid from "../../Uuid";
// import * as Models from "../../repositories/Models";
import * as graphql from "../../api/graphql";

export default async function updatePlayerName(
  executeQuery: Database.ExecuteQuery,
  input: graphql.RequireFields<
    graphql.MutationUpdatePlayerNameForInternetGameArgs,
    "name" | "playerToken"
  >
): Promise<boolean> {
  throw "todo";
  //   const playerToken = Uuid.generate();
  //   const internetGame = await InternetGameConfigurationRepository.findByJoinToken(
  //     executeQuery,
  //     input.joinGameToken
  //   );
  //   const newPlayer = await InternetGamePlayerRepository.create(
  //     executeQuery,
  //     internetGame.id,
  //     playerToken
  //   );
  //   const updatedPlayers: Models.PlayerConfiguration[] = [
  //     ...internetGame.players,
  //     {
  //       __typename: "PlayerConfiguration",
  //       playerId: newPlayer.id,
  //       name: "",
  //       color: { __typename: "Color", red: 0, green: 0, blue: 0 }
  //     }
  //   ];
  //   const updatedGame: Models.InternetGameConfiguration = {
  //     ...internetGame,
  //     players: updatedPlayers
  //   };
  //   await InternetGameConfigurationRepository.save(executeQuery, updatedGame);
  //   return playerToken;
}
