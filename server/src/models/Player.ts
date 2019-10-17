import * as Color from "./Color";

export const allowedColors = [
  Color.lightBrown,
  Color.brown,
  Color.darkBrown,
  Color.lightGreen,
  Color.green,
  Color.darkGreen,
  Color.lightBlue,
  Color.lightGray,
  Color.gray,
  Color.darkGray,
  Color.charcoal,
  Color.darkCharcoal,
  Color.lightOrange,
  Color.orange,
  Color.darkOrange,
  Color.lightPurple,
  Color.purple,
  Color.darkPurple,
  Color.lightRed,
  Color.red,
  Color.darkRed,
  Color.lightYellow,
  Color.darkYellow,
  Color.yellow,
  Color.white
];

// getNextAvailablePlayerColor(players)

export type PlayerConfiguration = {
  __typename: "PlayerConfiguration";
  color: Color.Color;
  playerId: string;
  name: string;
};
