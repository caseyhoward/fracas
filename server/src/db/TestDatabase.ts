import { Pool, QueryConfig, QueryResult, QueryResultRow } from "pg";
import * as Database from "../Database";

export const query: Database.ExecuteQuery = Database.postgres({
  user: "fracas",
  host: "localhost",
  database: "fracas-test",
  password: "abc123",
  port: 5432
});

export async function clean(query: Database.ExecuteQuery): Promise<void> {
  await query("TRUNCATE TABLE maps");
  await query("TRUNCATE TABLE games");
  await query("TRUNCATE TABLE internet_games");
  await query("TRUNCATE TABLE internet_game_players");
}
