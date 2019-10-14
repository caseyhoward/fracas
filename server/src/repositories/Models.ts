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
  __typename?: "NewGame";
  mapId: number;
  players: Array<Player>;
  neutralCountryTroops: Array<CountryTroopCounts>;
  playerTurn: PlayerTurn;
};

export interface InternetGame extends NewGame {
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
  __typename?: "PlayerConfiguration";
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
