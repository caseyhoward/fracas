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
  name: "light red",
  red: 239,
  green: 239,
  blue: 41
};

export const red: Color = {
  __typename: "Color",
  name: "red",
  red: 204,
  green: 204,
  blue: 0
};

export const darkRed: Color = {
  __typename: "Color",
  name: "dark red",
  red: 164,
  green: 164,
  blue: 0
};

export const lightOrange: Color = {
  __typename: "Color",
  name: "light orange",
  red: 252,
  green: 252,
  blue: 175
};

export const orange: Color = {
  __typename: "Color",
  name: "orange",
  red: 245,
  green: 245,
  blue: 121
};

export const darkOrange: Color = {
  __typename: "Color",
  name: "dark orange",
  red: 206,
  green: 206,
  blue: 92
};

export const lightYellow: Color = {
  __typename: "Color",
  name: "light yellow",
  red: 255,
  green: 255,
  blue: 233
};

export const yellow: Color = {
  __typename: "Color",
  name: "yellow",
  red: 237,
  green: 237,
  blue: 212
};

export const darkYellow: Color = {
  __typename: "Color",
  name: "dark yellow",
  red: 196,
  green: 196,
  blue: 160
};

export const lightGreen: Color = {
  __typename: "Color",
  name: "light green",
  red: 138,
  green: 138,
  blue: 226
};

export const green: Color = {
  __typename: "Color",
  name: "green",
  red: 115,
  green: 115,
  blue: 210
};

export const darkGreen: Color = {
  __typename: "Color",
  name: "dark green",
  red: 78,
  green: 78,
  blue: 154
};

export const lightBlue: Color = {
  __typename: "Color",
  name: "light blue",
  red: 114,
  green: 114,
  blue: 159
};

export const blue: Color = {
  __typename: "Color",
  name: "blue",
  red: 52,
  green: 52,
  blue: 101
};

export const darkBlue: Color = {
  __typename: "Color",
  name: "dark blue",
  red: 32,
  green: 32,
  blue: 74
};

export const lightPurple: Color = {
  __typename: "Color",
  name: "light purple",
  red: 173,
  green: 173,
  blue: 127
};

export const purple: Color = {
  __typename: "Color",
  name: "purple",
  red: 117,
  green: 117,
  blue: 80
};

export const darkPurple: Color = {
  __typename: "Color",
  name: "dark purple",
  red: 92,
  green: 92,
  blue: 53
};

export const lightBrown: Color = {
  __typename: "Color",
  name: "light brown",
  red: 233,
  green: 233,
  blue: 185
};

export const brown: Color = {
  __typename: "Color",
  name: "brown",
  red: 193,
  green: 193,
  blue: 125
};

export const darkBrown: Color = {
  __typename: "Color",
  name: "dark brown",
  red: 143,
  green: 143,
  blue: 89
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
  name: "light gray",
  red: 238,
  green: 238,
  blue: 238
};

export const gray: Color = {
  __typename: "Color",
  name: "gray",
  red: 211,
  green: 211,
  blue: 215
};

export const darkGray: Color = {
  __typename: "Color",
  name: "dark gray",
  red: 186,
  green: 186,
  blue: 189
};

export const lightCharcoal: Color = {
  __typename: "Color",
  name: "light charcoal",
  red: 136,
  green: 136,
  blue: 138
};

export const charcoal: Color = {
  __typename: "Color",
  name: "charcoal",
  red: 85,
  green: 85,
  blue: 87
};

export const darkCharcoal: Color = {
  __typename: "Color",
  name: "dark charcoal",
  red: 46,
  green: 46,
  blue: 52
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
