import { ExecuteQuery } from "../Database";
import * as InternetGamePlayer from "../InternetGamePlayer";

export interface Row {
  id: number;
  join_token?: string;
  map_id: number;
  game_json: string;
}

export type InternetGame = Configuration | Game;

export type Json = ConfigurationJson | GameJson;

export interface ConfigurationJson {
  __typename: "ConfigurationJson";
  players: Array<PlayerConfiguration>;
}

export interface GameJson {
  __typename: "GameJson";
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
}

export type Configuration = {
  __typename: "Configuration";
  id: number;
  players: Array<PlayerConfiguration>;
  mapId: number;
  joinToken: string;
};

export type NewConfiguration = {
  __typename: "NewConfiguration";
  players: Array<PlayerConfiguration>;
  mapId: number;
  joinToken: string;
};

export type CountryTroopCounts = {
  __typename: "CountryTroopCounts";
  countryId: string;
  troopCount: number;
};

export type Color = {
  __typename: "Color";
  red: number;
  green: number;
  blue: number;
};

export type Player = {
  __typename: "Player";
  id: string;
  name: string;
  countryTroopCounts: Array<CountryTroopCounts>;
  capitol?: string;
  color: Color;
  ports: string[];
};

export type NewGame = {
  __typename?: "NewGame";
  mapId: number;
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
};

export interface Game extends NewGame {
  __typename?: "NewGame";
  id: number;
  mapId: number;
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
}

export type PlayerTurn = {
  __typename?: "PlayerTurn";
  playerId: string;
  playerTurnStage: PlayerTurnStage;
  fromCountryId?: string;
  troopCount?: string;
};

export type PlayerConfiguration = {
  color: { red: number; green: number; blue: number };
  playerId: number;
  name: string;
};

export enum PlayerTurnStage {
  CapitolPlacement = "CapitolPlacement",
  TroopPlacement = "TroopPlacement",
  AttackAnnexOrPort = "AttackAnnexOrPort",
  TroopMovement = "TroopMovement",
  TroopMovementFromSelected = "TroopMovementFromSelected",
  GameOver = "GameOver"
}

export async function updateMap(
  executeQuery: ExecuteQuery,
  id: String,
  mapId: number
): Promise<void> {
  await executeQuery("UPDATE internet_games SET map_id = $1 WHERE id = $2", [
    mapId,
    id
  ]);
}

type NewInternetGame = NewGame | NewConfiguration;

export async function save(
  executeQuery: ExecuteQuery,
  internetGame: InternetGame
): Promise<void> {
  // const updatedRow: Row = internetGameToRow(internetGame);
  // if (internetGame.__typename ===)
  // await executeQuery(
  //   "UPDATE internet_games SET join_token = $1, map_id = $2, game_json = $3 WHERE id = $4",
  //   [
  //     internetGame.joinToken,
  //     internetGame.map_id,
  //     internetGame.game_json,
  //     internetGame.id
  //   ]
  // );
  throw "todo";
}

export async function create(
  executeQuery: ExecuteQuery,
  newInternetGame: NewInternetGame
): Promise<number> {
  if (newInternetGame.__typename === "NewConfiguration") {
    const configurationJson: ConfigurationJson = {
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
    return row.id;
  } else {
    throw "Can't create game";
  }
}

function addPlayer(executeQuery: ExecuteQuery, id: number, player: Player) {}

export async function findByPlayerToken(
  executeQuery: ExecuteQuery,
  playerToken: string
): Promise<InternetGame> {
  const player = await InternetGamePlayer.findByToken(
    executeQuery,
    playerToken
  );
  return findById(executeQuery, player.gameId);
}

// export async function findByJoinToken(
//   executeQuery: ExecuteQuery,
//   joinToken: string
// ): Promise<InternetGame> {
//   const result = await executeQuery(
//     "SELECT * FROM internet_games WHERE join_token = $1",
//     [joinToken]
//   );

//   const row: Row = result.rows[0];
//   return rowToInternetGame(row);
// }

export async function findById(
  executeQuery: ExecuteQuery,
  id: number
): Promise<InternetGame> {
  const result = await executeQuery(
    "SELECT * FROM internet_games WHERE id = $1",
    [id]
  );

  const row: Row = result.rows[0];
  return rowToInternetGame(row);
}

export function rowToInternetGame(row: Row): InternetGame {
  const json: Json = JSON.parse(row.game_json);
  if (json.__typename === "GameJson") {
    return <any>{
      __typename: "Game",
      id: row.id.toString(),
      players: json.players,
      neutralCountryTroops: json.neutralCountryTroops,
      playerTurn: json.playerTurn
    };
  } else if (json.__typename === "ConfigurationJson") {
    const configuration: Configuration = {
      __typename: "Configuration",
      id: row.id,
      mapId: row.map_id,
      players: json.players,
      joinToken: row.join_token || ""
    };
    return configuration;
  } else {
    throw "Bad JSON";
  }
}

// export function internetGameToRow(internetGame: InternetGame): Row {
//   let json: Json;
//   let jsonToken: string | undefined;
//   let mapId: number;
//   if (isConfiguring(internetGame)) {
//     jsonToken = internetGame.joinToken;
//     json = {
//       players: internetGame.players,
//       userPlayerId: internetGame.userPlayerId
//     };
//     mapId = parseInt(internetGame.mapId);
//   } else if (isGame(internetGame)) {
//     jsonToken = undefined;
//     json = {
//       players: internetGame.players,
//       playerTurn: internetGame.playerTurn,
//       neutralCountryTroops: internetGame.neutralCountryTroops
//     };
//     mapId = parseInt((<any>internetGame).mapId);
//   } else {
//     throw "Bad JSON";
//   }
//   return {
//     id: parseInt(internetGame.id.toString()),
//     join_token: jsonToken,
//     map_id: mapId,
//     game_json: JSON.stringify(json)
//   };
// }

// function isGameJson(json: Json): json is GameJson {
//   return (json as GameJson).neutralCountryTroops !== undefined;
// }

// function isConfiguringJson(json: Json): json is ConfigurationJson {
//   return (json as GameJson).neutralCountryTroops === undefined;
// }

// function isGame(internetGame: InternetGame): internetGame is Game.Game {
//   return (internetGame as Game.Game).id !== undefined;
// }

// export function isConfiguring(
//   internetGame: InternetGame
// ): internetGame is InternetGameConfiguration {
//   return (internetGame as Game.Game).neutralCountryTroops === undefined;
// }
