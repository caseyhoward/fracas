import * as Graphql from "../api/graphql";

export type Color = {
  __typename: "Color";
  name: string;
  red: number;
  green: number;
  blue: number;
};

export function isEqual(color1: Color, color2: Color): boolean {
  return (
    color1.red === color2.red &&
    color1.green === color2.green &&
    color1.blue === color2.blue
  );
}

export function fromColorInput(colorInput: Graphql.ColorInput): Color {
  const color = allColors.find(
    color =>
      color.red === colorInput.red &&
      color.green === colorInput.green &&
      color.blue === colorInput.blue
  );
  if (color) {
    return color;
  } else {
    throw "Invalid color";
  }
}

export const lightRed: Color = {
  __typename: "Color",
  name: "lightRed",
  red: 239,
  green: 41,
  blue: 41
};

export const red: Color = {
  __typename: "Color",
  name: "red",
  red: 204,
  green: 0,
  blue: 0
};

export const darkRed: Color = {
  __typename: "Color",
  name: "darkRed",
  red: 164,
  green: 0,
  blue: 0
};

export const lightOrange: Color = {
  __typename: "Color",
  name: "lightOrange",
  red: 252,
  green: 175,
  blue: 62
};

export const orange: Color = {
  __typename: "Color",
  name: "orange",
  red: 245,
  green: 121,
  blue: 0
};

export const darkOrange: Color = {
  __typename: "Color",
  name: "darkOrange",
  red: 206,
  green: 92,
  blue: 0
};

export const lightYellow: Color = {
  __typename: "Color",
  name: "lightYellow",
  red: 255,
  green: 233,
  blue: 79
};

export const yellow: Color = {
  __typename: "Color",
  name: "yellow",
  red: 237,
  green: 212,
  blue: 0
};

export const darkYellow: Color = {
  __typename: "Color",
  name: "darkYellow",
  red: 196,
  green: 160,
  blue: 0
};

export const lightGreen: Color = {
  __typename: "Color",
  name: "lightGreen",
  red: 138,
  green: 226,
  blue: 52
};

export const green: Color = {
  __typename: "Color",
  name: "green",
  red: 115,
  green: 210,
  blue: 22
};

export const darkGreen: Color = {
  __typename: "Color",
  name: "darkGreen",
  red: 78,
  green: 154,
  blue: 6
};

export const lightBlue: Color = {
  __typename: "Color",
  name: "lightBlue",
  red: 114,
  green: 159,
  blue: 207
};

export const blue: Color = {
  __typename: "Color",
  name: "blue",
  red: 52,
  green: 101,
  blue: 164
};

export const darkBlue: Color = {
  __typename: "Color",
  name: "darkBlue",
  red: 32,
  green: 74,
  blue: 135
};

export const lightPurple: Color = {
  __typename: "Color",
  name: "lightPurple",
  red: 173,
  green: 127,
  blue: 168
};

export const purple: Color = {
  __typename: "Color",
  name: "purple",
  red: 117,
  green: 80,
  blue: 123
};

export const darkPurple: Color = {
  __typename: "Color",
  name: "darkPurple",
  red: 92,
  green: 53,
  blue: 102
};

export const lightBrown: Color = {
  __typename: "Color",
  name: "lightBrown",
  red: 233,
  green: 185,
  blue: 110
};

export const brown: Color = {
  __typename: "Color",
  name: "brown",
  red: 193,
  green: 125,
  blue: 17
};

export const darkBrown: Color = {
  __typename: "Color",
  name: "darkBrown",
  red: 143,
  green: 89,
  blue: 2
};

export const black: Color = {
  __typename: "Color",
  name: "black",
  red: 0,
  green: 0,
  blue: 0
};

export const white: Color = {
  __typename: "Color",
  name: "white",
  red: 255,
  green: 255,
  blue: 255
};

export const lightGray: Color = {
  __typename: "Color",
  name: "lightGray",
  red: 238,
  green: 238,
  blue: 236
};

export const gray: Color = {
  __typename: "Color",
  name: "gray",
  red: 211,
  green: 215,
  blue: 207
};

export const darkGray: Color = {
  __typename: "Color",
  name: "darkGray",
  red: 186,
  green: 189,
  blue: 182
};

export const lightCharcoal: Color = {
  __typename: "Color",
  name: "lightCharcoal",
  red: 136,
  green: 138,
  blue: 133
};

export const charcoal: Color = {
  __typename: "Color",
  name: "charcoal",
  red: 85,
  green: 87,
  blue: 83
};

export const darkCharcoal: Color = {
  __typename: "Color",
  name: "darkCharcoal",
  red: 46,
  green: 52,
  blue: 54
};

const allColors = [
  black,
  blue,
  brown,
  charcoal,
  darkBlue,
  darkBrown,
  darkCharcoal,
  darkGray,
  darkGreen,
  darkOrange,
  darkPurple,
  darkRed,
  darkYellow,
  gray,
  green,
  lightBlue,
  lightBrown,
  lightCharcoal,
  lightGray,
  lightGreen,
  lightOrange,
  lightPurple,
  lightRed,
  lightYellow,
  orange,
  purple,
  red,
  white,
  yellow
];
