import * as Database from "./db";

export interface PlayerToken {}

export interface InternetGamePlayer {
  gameId: number;
}

export async function findByToken(
  executeQuery: Database.ExecuteQuery,
  playerToken: string
): Promise<InternetGamePlayer> {
  const result = await executeQuery(
    "SELECT * FROM internet_game_players WHERE token = $1",
    [playerToken]
  );
  return result.rows[0];
}
