/*************
 * MAP
 *************/

import * as Graphql from "../api/graphql";
import * as Player from "../models/Player";
import * as Color from "../models/Color";

export type BodyOfWater = {
  __typename?: "BodyOfWater";
  id: string;
  neighboringCountries: Array<string>;
};

export type Country = {
  __typename?: "Country";
  id: string;
  coordinates: Array<Point>;
  polygon: Array<Point>;
  waterEdges: Array<Segment>;
  center: Point;
  neighboringCountries: Array<string>;
  neighboringBodiesOfWater: Array<string>;
};

export type Dimensions = {
  __typename?: "Dimensions";
  width: number;
  height: number;
};

export type Map = {
  __typename?: "Map";
  id: string;
  name: string;
  countries: Array<Country>;
  bodiesOfWater: Array<BodyOfWater>;
  dimensions: Dimensions;
};

export type NewMap = {
  name: string;
  countries: Array<Country>;
  bodiesOfWater: Array<BodyOfWater>;
  dimensions: Dimensions;
};

export type Point = {
  __typename?: "Point";
  x: number;
  y: number;
};

export type Segment = {
  __typename?: "Segment";
  point1: Point;
  point2: Point;
};

/*************
 * GAME
 *************/

export type InternetGameOrConfiguration =
  | InternetGameConfiguration
  | InternetGame;

export type Json = ConfigurationJson | GameJson;

export interface ConfigurationJson {
  __typename: "ConfigurationJson";
  players: Array<Player.PlayerConfiguration>;
}

export interface GameJson {
  __typename: "GameJson";
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
}

export type InternetGameConfiguration = {
  __typename: "InternetGameConfiguration";
  id: string;
  players: Array<Player.PlayerConfiguration>;
  mapId: string;
  joinToken: string;
};

export type NewInternetGameConfiguration = {
  __typename: "NewInternetGameConfiguration";
  players: Array<Player.PlayerConfiguration>;
  mapId: string;
  joinToken: string;
};

export type CountryTroopCounts = {
  __typename: "CountryTroopCounts";
  countryId: string;
  troopCount: number;
};

export type Player = {
  __typename: "Player";
  id: string;
  name: string;
  countryTroopCounts: Array<CountryTroopCounts>;
  capitol?: string;
  color: Color.Color;
  ports: string[];
};

export type NewGame = {
  __typename: "NewGame";
  mapId: string;
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
};

export type InternetGame = {
  __typename: "InternetGame";
  id: string;
  mapId: string;
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
};

export type PlayerTurn = {
  __typename: "PlayerTurn";
  playerId: string;
  playerTurnStage: PlayerTurnStage;
  fromCountryId?: string;
  troopCount?: string;
};

export enum PlayerTurnStage {
  CapitolPlacement = "CapitolPlacement",
  TroopPlacement = "TroopPlacement",
  AttackAnnexOrPort = "AttackAnnexOrPort",
  TroopMovement = "TroopMovement",
  TroopMovementFromSelected = "TroopMovementFromSelected",
  GameOver = "GameOver"
}

export interface InternetGamePlayer {
  id: string;
  gameId: string;
  playerToken: string;
}

export function internetGameToGraphql(
  internetGame: InternetGame,
  currentUserPlayerId: string
): Graphql.Game {
  return {
    __typename: "Game",
    id: internetGame.id.toString(),
    currentUserPlayerId: currentUserPlayerId,
    map: <any>{ id: internetGame.mapId.toString() },
    neutralCountryTroops: internetGame.neutralCountryTroops,
    playerTurn: {
      ...internetGame.playerTurn,
      playerId: internetGame.playerTurn.playerId.toString()
    },
    players: internetGame.players.map(player => {
      return {
        ...player,
        id: player.id.toString(),
        __typename: "Player"
      };
    })
  };
}

export function internetGameConfigurationToGraphQl(
  currentUserPlayer: InternetGamePlayer,
  configuration: InternetGameConfiguration
): Graphql.InternetGameConfiguration {
  const players: Graphql.InternetGamePlayerConfiguration[] = configuration.players.map(
    player => {
      return {
        ...player,
        __typename: "InternetGamePlayerConfiguration",
        playerId: player.playerId
      };
    }
  );
  return {
    __typename: "InternetGameConfiguration",
    id: configuration.id,
    players: players,
    mapId: configuration.mapId.toString(),
    joinToken: configuration.joinToken,
    currentUserPlayerId: currentUserPlayer.id,
    isCurrentUserHost: Player.isCurrentUserHost(
      currentUserPlayer.id,
      configuration.players
    )
  };
}
