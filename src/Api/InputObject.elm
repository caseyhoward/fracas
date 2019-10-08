-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.InputObject exposing (..)

import Api.Enum.PlayerTurnStage
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


buildBodyOfWaterInput : BodyOfWaterInputRequiredFields -> BodyOfWaterInput
buildBodyOfWaterInput required =
    { id = required.id, neighboringCountries = required.neighboringCountries }


type alias BodyOfWaterInputRequiredFields =
    { id : String
    , neighboringCountries : List String
    }


{-| Type for the BodyOfWaterInput input object.
-}
type alias BodyOfWaterInput =
    { id : String
    , neighboringCountries : List String
    }


{-| Encode a BodyOfWaterInput into a value that can be used as an argument.
-}
encodeBodyOfWaterInput : BodyOfWaterInput -> Value
encodeBodyOfWaterInput input =
    Encode.maybeObject
        [ ( "id", Encode.string input.id |> Just ), ( "neighboringCountries", (Encode.string |> Encode.list) input.neighboringCountries |> Just ) ]


buildColorInput : ColorInputRequiredFields -> ColorInput
buildColorInput required =
    { red = required.red, green = required.green, blue = required.blue }


type alias ColorInputRequiredFields =
    { red : Int
    , green : Int
    , blue : Int
    }


{-| Type for the ColorInput input object.
-}
type alias ColorInput =
    { red : Int
    , green : Int
    , blue : Int
    }


{-| Encode a ColorInput into a value that can be used as an argument.
-}
encodeColorInput : ColorInput -> Value
encodeColorInput input =
    Encode.maybeObject
        [ ( "red", Encode.int input.red |> Just ), ( "green", Encode.int input.green |> Just ), ( "blue", Encode.int input.blue |> Just ) ]


buildCountryInput : CountryInputRequiredFields -> CountryInput
buildCountryInput required =
    CountryInput { id = required.id, coordinates = required.coordinates, polygon = required.polygon, waterEdges = required.waterEdges, center = required.center, neighboringCountries = required.neighboringCountries, neighboringBodiesOfWater = required.neighboringBodiesOfWater }


type alias CountryInputRequiredFields =
    { id : String
    , coordinates : List PointInput
    , polygon : List PointInput
    , waterEdges : List SegmentInput
    , center : PointInput
    , neighboringCountries : List String
    , neighboringBodiesOfWater : List String
    }


{-| Type alias for the `CountryInput` attributes. Note that this type
needs to use the `CountryInput` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias CountryInputRaw =
    { id : String
    , coordinates : List PointInput
    , polygon : List PointInput
    , waterEdges : List SegmentInput
    , center : PointInput
    , neighboringCountries : List String
    , neighboringBodiesOfWater : List String
    }


{-| Type for the CountryInput input object.
-}
type CountryInput
    = CountryInput CountryInputRaw


{-| Encode a CountryInput into a value that can be used as an argument.
-}
encodeCountryInput : CountryInput -> Value
encodeCountryInput (CountryInput input) =
    Encode.maybeObject
        [ ( "id", Encode.string input.id |> Just ), ( "coordinates", (encodePointInput |> Encode.list) input.coordinates |> Just ), ( "polygon", (encodePointInput |> Encode.list) input.polygon |> Just ), ( "waterEdges", (encodeSegmentInput |> Encode.list) input.waterEdges |> Just ), ( "center", encodePointInput input.center |> Just ), ( "neighboringCountries", (Encode.string |> Encode.list) input.neighboringCountries |> Just ), ( "neighboringBodiesOfWater", (Encode.string |> Encode.list) input.neighboringBodiesOfWater |> Just ) ]


buildCountryTroopCountsInput : CountryTroopCountsInputRequiredFields -> CountryTroopCountsInput
buildCountryTroopCountsInput required =
    { countryId = required.countryId, troopCount = required.troopCount }


type alias CountryTroopCountsInputRequiredFields =
    { countryId : String
    , troopCount : Int
    }


{-| Type for the CountryTroopCountsInput input object.
-}
type alias CountryTroopCountsInput =
    { countryId : String
    , troopCount : Int
    }


{-| Encode a CountryTroopCountsInput into a value that can be used as an argument.
-}
encodeCountryTroopCountsInput : CountryTroopCountsInput -> Value
encodeCountryTroopCountsInput input =
    Encode.maybeObject
        [ ( "countryId", Encode.string input.countryId |> Just ), ( "troopCount", Encode.int input.troopCount |> Just ) ]


buildDimensionsInput : DimensionsInputRequiredFields -> DimensionsInput
buildDimensionsInput required =
    { width = required.width, height = required.height }


type alias DimensionsInputRequiredFields =
    { width : Int
    , height : Int
    }


{-| Type for the DimensionsInput input object.
-}
type alias DimensionsInput =
    { width : Int
    , height : Int
    }


{-| Encode a DimensionsInput into a value that can be used as an argument.
-}
encodeDimensionsInput : DimensionsInput -> Value
encodeDimensionsInput input =
    Encode.maybeObject
        [ ( "width", Encode.int input.width |> Just ), ( "height", Encode.int input.height |> Just ) ]


buildGameInput : GameInputRequiredFields -> GameInput
buildGameInput required =
    GameInput { mapId = required.mapId, players = required.players, neutralCountryTroops = required.neutralCountryTroops, numberOfPlayers = required.numberOfPlayers, playerTurn = required.playerTurn }


type alias GameInputRequiredFields =
    { mapId : String
    , players : List PlayerInput
    , neutralCountryTroops : List CountryTroopCountsInput
    , numberOfPlayers : Int
    , playerTurn : PlayerTurnInput
    }


{-| Type alias for the `GameInput` attributes. Note that this type
needs to use the `GameInput` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias GameInputRaw =
    { mapId : String
    , players : List PlayerInput
    , neutralCountryTroops : List CountryTroopCountsInput
    , numberOfPlayers : Int
    , playerTurn : PlayerTurnInput
    }


{-| Type for the GameInput input object.
-}
type GameInput
    = GameInput GameInputRaw


{-| Encode a GameInput into a value that can be used as an argument.
-}
encodeGameInput : GameInput -> Value
encodeGameInput (GameInput input) =
    Encode.maybeObject
        [ ( "mapId", Encode.string input.mapId |> Just ), ( "players", (encodePlayerInput |> Encode.list) input.players |> Just ), ( "neutralCountryTroops", (encodeCountryTroopCountsInput |> Encode.list) input.neutralCountryTroops |> Just ), ( "numberOfPlayers", Encode.int input.numberOfPlayers |> Just ), ( "playerTurn", encodePlayerTurnInput input.playerTurn |> Just ) ]


buildMapInput : MapInputRequiredFields -> MapInput
buildMapInput required =
    MapInput { name = required.name, countries = required.countries, bodiesOfWater = required.bodiesOfWater, dimensions = required.dimensions }


type alias MapInputRequiredFields =
    { name : String
    , countries : List CountryInput
    , bodiesOfWater : List BodyOfWaterInput
    , dimensions : DimensionsInput
    }


{-| Type alias for the `MapInput` attributes. Note that this type
needs to use the `MapInput` type (not just a plain type alias) because it has
references to itself either directly (recursive) or indirectly (circular). See
<https://github.com/dillonkearns/elm-graphql/issues/33>.
-}
type alias MapInputRaw =
    { name : String
    , countries : List CountryInput
    , bodiesOfWater : List BodyOfWaterInput
    , dimensions : DimensionsInput
    }


{-| Type for the MapInput input object.
-}
type MapInput
    = MapInput MapInputRaw


{-| Encode a MapInput into a value that can be used as an argument.
-}
encodeMapInput : MapInput -> Value
encodeMapInput (MapInput input) =
    Encode.maybeObject
        [ ( "name", Encode.string input.name |> Just ), ( "countries", (encodeCountryInput |> Encode.list) input.countries |> Just ), ( "bodiesOfWater", (encodeBodyOfWaterInput |> Encode.list) input.bodiesOfWater |> Just ), ( "dimensions", encodeDimensionsInput input.dimensions |> Just ) ]


buildPlayerInput : PlayerInputRequiredFields -> (PlayerInputOptionalFields -> PlayerInputOptionalFields) -> PlayerInput
buildPlayerInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { capitol = Absent }
    in
    { id = required.id, name = required.name, countryTroopCounts = required.countryTroopCounts, capitol = optionals.capitol, color = required.color, ports = required.ports }


type alias PlayerInputRequiredFields =
    { id : String
    , name : String
    , countryTroopCounts : List CountryTroopCountsInput
    , color : ColorInput
    , ports : List String
    }


type alias PlayerInputOptionalFields =
    { capitol : OptionalArgument String }


{-| Type for the PlayerInput input object.
-}
type alias PlayerInput =
    { id : String
    , name : String
    , countryTroopCounts : List CountryTroopCountsInput
    , capitol : OptionalArgument String
    , color : ColorInput
    , ports : List String
    }


{-| Encode a PlayerInput into a value that can be used as an argument.
-}
encodePlayerInput : PlayerInput -> Value
encodePlayerInput input =
    Encode.maybeObject
        [ ( "id", Encode.string input.id |> Just ), ( "name", Encode.string input.name |> Just ), ( "countryTroopCounts", (encodeCountryTroopCountsInput |> Encode.list) input.countryTroopCounts |> Just ), ( "capitol", Encode.string |> Encode.optional input.capitol ), ( "color", encodeColorInput input.color |> Just ), ( "ports", (Encode.string |> Encode.list) input.ports |> Just ) ]


buildPlayerTurnInput : PlayerTurnInputRequiredFields -> PlayerTurnInput
buildPlayerTurnInput required =
    { playerId = required.playerId, playerTurnStage = required.playerTurnStage }


type alias PlayerTurnInputRequiredFields =
    { playerId : String
    , playerTurnStage : Api.Enum.PlayerTurnStage.PlayerTurnStage
    }


{-| Type for the PlayerTurnInput input object.
-}
type alias PlayerTurnInput =
    { playerId : String
    , playerTurnStage : Api.Enum.PlayerTurnStage.PlayerTurnStage
    }


{-| Encode a PlayerTurnInput into a value that can be used as an argument.
-}
encodePlayerTurnInput : PlayerTurnInput -> Value
encodePlayerTurnInput input =
    Encode.maybeObject
        [ ( "playerId", Encode.string input.playerId |> Just ), ( "playerTurnStage", Encode.enum Api.Enum.PlayerTurnStage.toString input.playerTurnStage |> Just ) ]


buildPointInput : PointInputRequiredFields -> PointInput
buildPointInput required =
    { x = required.x, y = required.y }


type alias PointInputRequiredFields =
    { x : Int
    , y : Int
    }


{-| Type for the PointInput input object.
-}
type alias PointInput =
    { x : Int
    , y : Int
    }


{-| Encode a PointInput into a value that can be used as an argument.
-}
encodePointInput : PointInput -> Value
encodePointInput input =
    Encode.maybeObject
        [ ( "x", Encode.int input.x |> Just ), ( "y", Encode.int input.y |> Just ) ]


buildSegmentInput : SegmentInputRequiredFields -> SegmentInput
buildSegmentInput required =
    { point1 = required.point1, point2 = required.point2 }


type alias SegmentInputRequiredFields =
    { point1 : PointInput
    , point2 : PointInput
    }


{-| Type for the SegmentInput input object.
-}
type alias SegmentInput =
    { point1 : PointInput
    , point2 : PointInput
    }


{-| Encode a SegmentInput into a value that can be used as an argument.
-}
encodeSegmentInput : SegmentInput -> Value
encodeSegmentInput input =
    Encode.maybeObject
        [ ( "point1", encodePointInput input.point1 |> Just ), ( "point2", encodePointInput input.point2 |> Just ) ]
