import { ExecuteQuery } from "../Database";
import * as Models from "./Models";
import * as Player from "../models/Player";

export type FindById = (
  id: string
) => Promise<Models.InternetGameConfiguration>;

type FindByIdConstructor = (executeQuery: ExecuteQuery) => FindById;

export async function create(
  executeQuery: ExecuteQuery,
  newInternetGame: Models.NewInternetGameConfiguration
): Promise<string> {
  const configurationJson: Models.ConfigurationJson = {
    __typename: "ConfigurationJson",
    players: newInternetGame.players
  };
  const internetGameResult = await executeQuery(
    "INSERT INTO internet_games(join_token, map_id, game_json) VALUES ($1, $2, $3) RETURNING *",
    [
      newInternetGame.joinToken,
      newInternetGame.mapId,
      JSON.stringify(configurationJson)
    ]
  );
  const row: Row = internetGameResult.rows[0];
  return row.id.toString();
}

type UpdateMapConstructor = (executeQuery: ExecuteQuery) => UpdateMap;

export type UpdateMap = (id: string, mapId: string) => Promise<void>;

export const updateMap: UpdateMapConstructor = (executeQuery: ExecuteQuery) => {
  return async (id: string, mapId: string): Promise<void> => {
    return await executeQuery(
      "UPDATE internet_games SET map_id = $1 WHERE id = $2",
      [mapId, id]
    ).then(() => undefined);
  };
};

export async function save(
  executeQuery: ExecuteQuery,
  internetGame: Models.InternetGameConfiguration
): Promise<void> {
  const configurationJson: Models.ConfigurationJson = {
    __typename: "ConfigurationJson",
    players: internetGame.players
  };
  await executeQuery(
    "UPDATE internet_games SET join_token = $1, map_id = $2, game_json = $3 WHERE id = $4",
    [
      internetGame.joinToken,
      internetGame.mapId,
      configurationJson,
      internetGame.id
    ]
  );
}

export async function addPlayer(
  executeQuery: ExecuteQuery,
  id: string,
  player: Player.PlayerConfiguration
): Promise<void> {
  const configuration = await findById(executeQuery)(id);
  const updatedConfiguration = {
    ...configuration,
    players: [...configuration.players, player]
  };
  await save(executeQuery, updatedConfiguration);
}

export async function findByJoinToken(
  executeQuery: ExecuteQuery,
  joinToken: string
): Promise<Models.InternetGameConfiguration> {
  const result = await executeQuery(
    "SELECT * FROM internet_games WHERE join_token = $1",
    [joinToken]
  );

  const row: Row | undefined = result.rows[0];
  return rowToInternetGameConfiguration(row);
}

export const findById: FindByIdConstructor = (executeQuery: ExecuteQuery) => {
  return async (id: string) => {
    const result = await executeQuery(
      "SELECT * FROM internet_games WHERE id = $1",
      [id]
    );
    const row: Row | undefined = result.rows[0];
    return rowToInternetGameConfiguration(row);
  };
};

export function rowToInternetGameConfiguration(
  row: Row | undefined
): Models.InternetGameConfiguration {
  if (row) {
    const json: Models.Json = JSON.parse(row.game_json);
    if (json.__typename === "GameJson") {
      throw "Game already started";
    } else if (json.__typename === "ConfigurationJson") {
      const configuration: Models.InternetGameConfiguration = {
        __typename: "InternetGameConfiguration",
        id: row.id.toString(),
        mapId: row.map_id.toString(),
        players: json.players,
        joinToken: row.join_token || ""
      };
      return configuration;
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
