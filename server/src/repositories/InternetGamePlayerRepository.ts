import * as Database from "../Database";
import * as Models from "./Models";

interface Row {
  id: number;
  internet_game_id: number;
  token: string;
}

export async function create(
  executeQuery: Database.ExecuteQuery,
  gameId: number,
  playerToken: string
): Promise<Models.InternetGamePlayer> {
  const result = await executeQuery(
    "INSERT INTO internet_game_players (internet_game_id, token) VALUES ($1, $2) RETURNING *",
    [gameId, playerToken]
  );
  const row: Row = result.rows[0];
  return { id: row.id, gameId: gameId };
}

export async function findByToken(
  executeQuery: Database.ExecuteQuery,
  playerToken: string
): Promise<Models.InternetGamePlayer> {
  const result = await executeQuery(
    "SELECT * FROM internet_game_players WHERE token = $1",
    [playerToken]
  );
  const row: Row | undefined = result.rows[0];
  if (row) {
    return { id: row.id, gameId: row.internet_game_id, playerToken: row.token };
  } else {
    throw "Count not find game player by token";
  }
}
