/*************
 * MAP
 *************/

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
  id: number;
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
  players: Array<PlayerConfiguration>;
}

export interface GameJson {
  __typename: "GameJson";
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
}

export type InternetGameConfiguration = {
  __typename: "InternetGameConfiguration";
  id: number;
  players: Array<PlayerConfiguration>;
  mapId: number;
  joinToken: string;
};

export type NewInternetGameConfiguration = {
  __typename: "NewInternetGameConfiguration";
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
  __typename: "NewGame";
  mapId: number;
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
};

export type InternetGame = {
  __typename: "InternetGame";
  id: number;
  mapId: number;
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

export type PlayerConfiguration = {
  __typename: "PlayerConfiguration";
  color: Color;
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

export interface InternetGamePlayer {
  id: number;
  gameId: number;
  playerToken?: string;
}
