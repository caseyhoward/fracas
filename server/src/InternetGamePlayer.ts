import * as Database from "./db";

export interface PlayerToken {}

export interface InternetGamePlayer {
  gameId: number;
}

export async function findByToken(
  executeQuery: Database.ExecuteQuery,
  playerToken: PlayerToken
): Promise<InternetGamePlayer> {
  const result = await executeQuery("");
  return result.rows[0];
}
