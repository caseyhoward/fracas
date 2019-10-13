import * as Database from "./db";

interface Row {
  id: number;
  internet_game_id: number;
  token: string;
}

export interface InternetGamePlayer {
  id: number;
  gameId: number;
}

export async function findGameIdByToken(
  executeQuery: Database.ExecuteQuery,
  playerToken: string
): Promise<InternetGamePlayer> {
  const result = await executeQuery(
    "SELECT * FROM internet_game_players WHERE token = $1",
    [playerToken]
  );
  const row: Row = result.rows[0];
  return { id: row.id, gameId: row.internet_game_id };
}

export async function create(
  executeQuery: Database.ExecuteQuery,
  gameId: string,
  playerToken: string
): Promise<void> {
  await executeQuery(
    "INSERT INTO internet_game_players (internet_game_id, token, player_json) VALUES ($1, $2, $3)",
    [gameId, playerToken, ""]
  );
}
