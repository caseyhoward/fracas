import * as Color from "./Color";

export const allowedColors = [
  Color.lightGreen,
  Color.lightYellow,
  Color.darkGreen,
  Color.orange,
  Color.brown,
  Color.lightPurple,
  Color.green,
  Color.lightBlue,
  Color.lightGray,
  Color.gray,
  Color.darkGray,
  Color.charcoal,
  Color.lightBrown,
  Color.darkCharcoal,
  Color.lightOrange,
  Color.darkRed,
  Color.darkOrange,
  Color.purple,
  Color.darkBrown,
  Color.darkPurple,
  Color.lightRed,
  Color.red,
  Color.darkYellow,
  Color.yellow,
  Color.white
];

export type PlayerConfiguration = {
  __typename: "PlayerConfiguration";
  color: Color.Color;
  playerId: string;
  name: string;
};

export function getNextAvailablePlayerColor(
  players: PlayerConfiguration[]
): Color.Color {
  const playerColors = players.map(player => player.color);
  const availableColor = allowedColors.find(
    color =>
      !playerColors.find(playerColor => Color.isEqual(color, playerColor))
  );
  if (availableColor) {
    return availableColor;
  } else {
    throw "All colors used. There must be a bug checking for too many players.";
  }
}

export function createHost(id: string): PlayerConfiguration {
  return {
    __typename: "PlayerConfiguration",
    color: defaultHostColor,
    name: "Host",
    playerId: id
  };
}

export function isCurrentUserHost(
  playerId: string,
  players: PlayerConfiguration[]
): boolean {
  return players[0].playerId === playerId;
}

const defaultHostColor: Color.Color = Color.lightGreen;
