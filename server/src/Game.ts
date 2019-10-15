import { Game, NewGameInput, GameInput } from "./api/graphql";
export { Game, NewGameInput } from "./api/graphql";

import * as Database from "./Database";

function gameInputToGameRow(gameInput: NewGameInput): NewGameRow {
  const gameJson = {
    players: gameInput.players,
    playerTurn: gameInput.playerTurn,
    neutralCountryTroops: gameInput.neutralCountryTroops
  };

  return {
    map_id: parseInt(gameInput.mapId),
    game_json: JSON.stringify(gameJson)
  };
}

interface GameRow {
  id: number;
  map_id: number;
  game_json: string;
}

interface NewGameRow {
  map_id: number;
  game_json: string;
}

export async function get(
  executeQuery: Database.ExecuteQuery,
  id: number
): Promise<Game> {
  const result = await executeQuery("SELECT * FROM games WHERE id = $1", [id]);
  return gameToJson(result.rows[0]);
}

function gameToJson(gameRow: GameRow): Game {
  const gameWithoutId: Game = JSON.parse(gameRow.game_json);

  return {
    ...gameWithoutId,
    id: gameRow.id.toString(),
    map: <any>{ id: gameRow.map_id.toString() }
  };
}

export async function create(
  executeQuery: Database.ExecuteQuery,
  newGameInput: NewGameInput
): Promise<Game> {
  const newGameRow = gameInputToGameRow(newGameInput);
  const result = await executeQuery(
    "INSERT INTO games(map_id, game_json) VALUES ($1, $2) RETURNING *",
    [newGameRow.map_id, newGameRow.game_json]
  );
  return gameToJson(result.rows[0]);
}

export async function update(
  executeQuery: Database.ExecuteQuery,
  game: GameInput
) {
  const result = await executeQuery(
    "UPDATE games SET game_json = $2 WHERE id = $1 RETURNING *",
    [parseInt(game.id, 10), JSON.stringify(game)]
  );
  return gameToJson(result.rows[0]);
}
