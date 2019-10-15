import { ExecuteQuery } from "../Database";
import * as Models from "./Models";

export async function save(
  executeQuery: ExecuteQuery,
  internetGame: Models.InternetGame
): Promise<void> {
  const gameJson: Models.GameJson = {
    __typename: "GameJson",
    players: internetGame.players,
    neutralCountryTroops: internetGame.neutralCountryTroops,
    playerTurn: internetGame.playerTurn
  };
  await executeQuery("UPDATE internet_games SET game_json = $1 WHERE id = $2", [
    JSON.stringify(gameJson),
    internetGame.id
  ]);
}

export async function findById(
  executeQuery: ExecuteQuery,
  id: number
): Promise<Models.InternetGame> {
  const result = await executeQuery(
    "SELECT * FROM internet_games WHERE id = $1",
    [id]
  );
  const row: Row | undefined = result.rows[0];
  return rowToInternetGame(row);
}

export function rowToInternetGame(row: Row | undefined): Models.InternetGame {
  if (row) {
    const json: Models.Json = JSON.parse(row.game_json);
    if (json.__typename === "GameJson") {
      const game: Models.InternetGame = {
        __typename: "InternetGame",
        players: json.players,
        id: row.id,
        mapId: row.map_id,
        neutralCountryTroops: json.neutralCountryTroops,
        playerTurn: json.playerTurn
      };
      return game;
    } else if (json.__typename === "ConfigurationJson") {
      throw "Still configuring";
    } else {
      throw "Bad JSON";
    }
  } else {
    throw "Not found";
  }
}

export interface Row {
  id: number;
  join_token?: string;
  map_id: number;
  game_json: string;
}