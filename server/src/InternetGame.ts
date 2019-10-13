import * as Uuid from "./Uuid";
import { ExecuteQuery } from "./db";
import * as Map from "./Map";
import * as Game from "./Game";
import {
  InternetGame,
  InternetGamePlayerConfiguration,
  InternetGameConfiguration,
  Player,
  CountryTroopCounts,
  PlayerTurn
} from "./api/graphql";
export { Game, NewGameInput } from "./api/graphql";

import * as InternetGamePlayer from "./InternetGamePlayer";

interface Row extends NewRow {
  id: number;
  join_token?: string;
  map_id: number;
  game_json: string;
}

interface NewRow {
  join_token?: string;
  map_id: number;
  game_json: string;
}

export async function create(executeQuery: ExecuteQuery): Promise<string> {
  const mapId = await Map.findFirstId(executeQuery);
  const hostToken = Uuid.generate();
  const joinToken = Uuid.generate();
  const newRowGameJson: ConfigurationJson = {
    players: [],
    mapId: mapId.toString(),
    userPlayerId: 1
  };
  const newRow: NewRow = {
    join_token: joinToken,
    map_id: mapId,
    game_json: JSON.stringify(newRowGameJson)
  };
  const internetGame = await executeQuery(
    "INSERT INTO internet_games(join_token, map_id, game_json) VALUES ($1, $2, $3) RETURNING *",
    [Uuid.generate(), newRow.map_id, newRow.game_json]
  );
  InternetGamePlayer.create(executeQuery, internetGame.rows[0].id, hostToken);

  return hostToken;
}

export async function findByPlayerToken(
  executeQuery: ExecuteQuery,
  playerToken: string
): Promise<InternetGame> {
  const player = await InternetGamePlayer.findGameIdByToken(
    executeQuery,
    playerToken
  );

  const result = await executeQuery(
    "SELECT * FROM internet_games WHERE id = $1",
    [player.gameId]
  );

  const row: Row = result.rows[0];
  return rowToInternetGame(row, player.id);
}

function rowToInternetGame(row: Row, userPlayerId: number): InternetGame {
  console.log("************", row.game_json);
  const json: Json = JSON.parse(row.game_json);
  if (isGameJson(json)) {
    return {
      __typename: "Game",
      id: row.id.toString(),
      map: <any>{ id: json.mapId },
      players: json.players,
      neutralCountryTroops: json.neutralCountryTroops,
      playerTurn: json.playerTurn
    };
  } else if (isConfiguringJson(json)) {
    const configuration: InternetGameConfiguration = {
      __typename: "InternetGameConfiguration",
      id: row.id,
      mapId: json.mapId,
      players: json.players,
      joinToken: row.join_token || "",
      userPlayerId: userPlayerId
    };
    return configuration;
  } else {
    throw "Bad JSON";
  }
}

type Json = ConfigurationJson | GameJson;

interface ConfigurationJson {
  players: InternetGamePlayerConfiguration[];
  mapId: string;
  userPlayerId: number;
}

interface GameJson {
  mapId: string;
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
}

function internetGameToRow(internetGame: InternetGame): Row {
  let json: Json;
  let jsonToken: string | undefined;
  if (isConfiguring(internetGame)) {
    jsonToken = internetGame.joinToken;
    json = {
      players: internetGame.players,
      mapId: internetGame.mapId,
      userPlayerId: internetGame.userPlayerId
    };
  } else if (isGame(internetGame)) {
    jsonToken = undefined;
    json = {
      mapId: internetGame.map.id,
      players: internetGame.players,
      playerTurn: internetGame.playerTurn,
      neutralCountryTroops: internetGame.neutralCountryTroops
    };
  } else {
    throw "Bad JSON";
  }
  return {
    id: parseInt(internetGame.id.toString()),
    join_token: jsonToken,
    map_id: parseInt(json.mapId, 10),
    game_json: JSON.stringify(json)
  };
}

function isGame(internetGame: InternetGame): internetGame is Game.Game {
  return (internetGame as Game.Game).id !== undefined;
}

function isConfiguring(
  internetGame: InternetGame
): internetGame is InternetGameConfiguration {
  return (internetGame as Game.Game).id === undefined;
}

function isGameJson(json: Json): json is GameJson {
  return (json as GameJson).neutralCountryTroops !== undefined;
}
function isConfiguringJson(json: Json): json is ConfigurationJson {
  return (json as GameJson).neutralCountryTroops === undefined;
}