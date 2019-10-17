-- ElmColor


module Colors exposing
    ( Color
    , black
    , blue
    , brown
    , charcoal
    , darkBlue
    , darkBrown
    , darkCharcoal
    , darkGray
    , darkGreen
    , darkOrange
    , darkPurple
    , darkRed
    , darkYellow
    , gray
    , green
    , input
    , lightBlue
    , lightBrown
    , lightCharcoal
    , lightGray
    , lightGreen
    , lightOrange
    , lightPurple
    , lightRed
    , lightYellow
    , orange
    , purple
    , red
    , rgb255
    , selectionSet
    , toColor
    , toElementColor
    , transparency
    , white
    , yellow
    )

import Api.InputObject
import Api.Object
import Api.Object.Color
import Color as ElmColor
import Element
import Graphql.SelectionSet exposing (SelectionSet)


type alias Color =
    { red : Int
    , green : Int
    , blue : Int
    }


toColor : Color -> ElmColor.Color
toColor color =
    ElmColor.rgb255 color.red color.green color.blue


transparency : Float -> ElmColor.Color
transparency value =
    ElmColor.rgba 0 0 0 value


selectionSet : SelectionSet Color Api.Object.Color
selectionSet =
    Graphql.SelectionSet.map3 Color
        Api.Object.Color.red
        Api.Object.Color.green
        Api.Object.Color.blue


toElementColor : Color -> Element.Color
toElementColor color =
    Element.rgb255 color.red color.green color.blue


input : Color -> Api.InputObject.ColorInput
input color =
    color


rgb255 : Int -> Int -> Int -> Color
rgb255 r g b =
    Color r g b


lightRed : Color
lightRed =
    Color 239 41 41


red : Color
red =
    Color 204 0 0


darkRed : Color
darkRed =
    Color 164 0 0


lightOrange : Color
lightOrange =
    Color 252 175 62


orange : Color
orange =
    Color 245 121 0


darkOrange : Color
darkOrange =
    Color 206 92 0


lightYellow : Color
lightYellow =
    Color 255 233 79


yellow : Color
yellow =
    Color 237 212 0


darkYellow : Color
darkYellow =
    Color 196 160 0


lightGreen : Color
lightGreen =
    Color 138 226 52


green : Color
green =
    Color 115 210 22


darkGreen : Color
darkGreen =
    Color 78 154 6


lightBlue : Color
lightBlue =
    Color 114 159 207


blue : Color
blue =
    Color 52 101 164


darkBlue : Color
darkBlue =
    Color 32 74 135


lightPurple : Color
lightPurple =
    Color 173 127 168


purple : Color
purple =
    Color 117 80 123


darkPurple : Color
darkPurple =
    Color 92 53 102


lightBrown : Color
lightBrown =
    Color 233 185 110


brown : Color
brown =
    Color 193 125 17


darkBrown : Color
darkBrown =
    Color 143 89 2


black : Color
black =
    Color 0 0 0


white : Color
white =
    Color 255 255 255



lightGray : Color
lightGray =
    Color 238 238 236


gray : Color
gray =
    Color 211 215 207


darkGray : Color
darkGray =
    Color 186 189 182


lightCharcoal : Color
lightCharcoal =
    Color 136 138 133


charcoal : Color
charcoal =
    Color 85 87 83


darkCharcoal : Color
darkCharcoal =
    Color 46 52 54
