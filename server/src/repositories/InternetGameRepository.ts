import { ExecuteQuery } from "../Database";
import * as InternetGamePlayerRepository from "./InternetGamePlayerRepository";
import * as Models from "./Models";

// export async function create(
//   executeQuery: ExecuteQuery,
//   newInternetGame: Models.InternetGame
// ): Promise<number> {
//   const configurationJson: Models.ConfigurationJson = {
//     __typename: "ConfigurationJson",
//     players: newInternetGame.players.map(player => {
//       return {
//         ...player,
//         playerId: parseInt(player.id, 10),
//         __typename: "PlayerConfiguration"
//       };
//     })
//   };
//   const internetGameResult = await executeQuery(
//     "UPDATE internet_games SET game_json = $1 WHERE id = $2",
//     [JSON.stringify(configurationJson), newInternetGame.id]
//   );
//   const row: Row = internetGameResult.rows[0];
//   return row.id;
// }

export interface Row {
  id: number;
  join_token?: string;
  map_id: number;
  game_json: string;
}

export async function updateMap(
  executeQuery: ExecuteQuery,
  id: String,
  mapId: number
): Promise<void> {
  await executeQuery("UPDATE internet_games SET map_id = $1 WHERE id = $2", [
    mapId,
    id
  ]);
}

export async function save(
  executeQuery: ExecuteQuery,
  internetGame: Models.InternetGame
): Promise<void> {
  // const updatedRow: Row = internetGameToRow(internetGame);
  // if (internetGame.__typename ===)
  // await executeQuery(
  //   "UPDATE internet_games SET join_token = $1, map_id = $2, game_json = $3 WHERE id = $4",
  //   [
  //     internetGame.joinToken,
  //     internetGame.map_id,
  //     internetGame.game_json,
  //     internetGame.id
  //   ]
  // );
  throw "todo";
}

export async function addPlayer(
  executeQuery: ExecuteQuery,
  id: number,
  player: Models.PlayerConfiguration
): Promise<void> {
  const game = findById(executeQuery, id);
}

// export async function findByPlayerToken(
//   executeQuery: ExecuteQuery,
//   playerToken: string
// ): Promise<Models.InternetGameOrConfiguration> {
//   const player = await InternetGamePlayer.findByToken(
//     executeQuery,
//     playerToken
//   );
//   return findById(executeQuery, player.gameId);
// }

// export async function findByJoinToken(
//   executeQuery: ExecuteQuery,
//   joinToken: string
// ): Promise<InternetGame> {
//   const result = await executeQuery(
//     "SELECT * FROM internet_games WHERE join_token = $1",
//     [joinToken]
//   );

//   const row: Row = result.rows[0];
//   return rowToInternetGame(row);
// }

export async function findById(
  executeQuery: ExecuteQuery,
  id: number
): Promise<Models.InternetGameOrConfiguration> {
  const result = await executeQuery(
    "SELECT * FROM internet_games WHERE id = $1",
    [id]
  );

  const row: Row = result.rows[0];
  return rowToInternetGame(row);
}

export function rowToInternetGame(row: Row): Models.InternetGame {
  const json: Models.Json = JSON.parse(row.game_json);
  if (json.__typename === "GameJson") {
    return {
      __typename: "InternetGame",
      id: row.id,
      mapId: row.map_id,
      players: json.players,
      neutralCountryTroops: json.neutralCountryTroops,
      playerTurn: json.playerTurn
    };
  } else {
    throw "Bad JSON";
  }
}

// export function internetGameToRow(internetGame: InternetGame): Row {
//   let json: Json;
//   let jsonToken: string | undefined;
//   let mapId: number;
//   if (isConfiguring(internetGame)) {
//     jsonToken = internetGame.joinToken;
//     json = {
//       players: internetGame.players,
//       userPlayerId: internetGame.userPlayerId
//     };
//     mapId = parseInt(internetGame.mapId);
//   } else if (isGame(internetGame)) {
//     jsonToken = undefined;
//     json = {
//       players: internetGame.players,
//       playerTurn: internetGame.playerTurn,
//       neutralCountryTroops: internetGame.neutralCountryTroops
//     };
//     mapId = parseInt((<any>internetGame).mapId);
//   } else {
//     throw "Bad JSON";
//   }
//   return {
//     id: parseInt(internetGame.id.toString()),
//     join_token: jsonToken,
//     map_id: mapId,
//     game_json: JSON.stringify(json)
//   };
// }

// function isGameJson(json: Json): json is GameJson {
//   return (json as GameJson).neutralCountryTroops !== undefined;
// }

// function isConfiguringJson(json: Json): json is ConfigurationJson {
//   return (json as GameJson).neutralCountryTroops === undefined;
// }

// function isGame(internetGame: InternetGame): internetGame is Game.Game {
//   return (internetGame as Game.Game).id !== undefined;
// }

// export function isConfiguring(
//   internetGame: InternetGame
// ): internetGame is InternetGameConfiguration {
//   return (internetGame as Game.Game).neutralCountryTroops === undefined;
// }
