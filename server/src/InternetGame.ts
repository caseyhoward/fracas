import * as Uuid from "./Uuid";
import { ExecuteQuery } from "./db";
import * as Map from "./Map";
// import { PlayerToken } from "./api/graphql";

export interface PlayerToken {
  kind: "player_token";
  value: string;
}

export interface InternetGame {}

export async function create(executeQuery: ExecuteQuery): Promise<string> {
  const mapId = await Map.findFirstId(executeQuery);
  const hostToken = Uuid.generate();
  console.log([Uuid.generate(), mapId, ""]);
  const internetGame = await executeQuery(
    "INSERT INTO internet_games(join_token, map_id, game_json) VALUES ($1, $2, $3) RETURNING *",
    [Uuid.generate(), mapId, ""]
  );
  await executeQuery(
    "INSERT INTO internet_game_players (internet_game_id, token, player_json) VALUES ($1, $2, $3)",
    [internetGame.rows[0].id, hostToken, ""]
  );

  return hostToken;
}

export function findById(
  executeQuery: ExecuteQuery,
  id: number
): Promise<InternetGame> {
  return Promise.resolve({});
}

function playerToken(token: string): PlayerToken {
  return { kind: "player_token", value: token };
}
