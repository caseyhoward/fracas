module Map exposing
    ( Id
    , Map
    , create
    , idToString
    , parse
    , urlParser
    )

import Api.InputObject
import Api.Mutation
import Api.Object.Map
import Collage
import Dict
import GameMap
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Encode
import RemoteData
import Set
import Url.Parser
import ViewHelpers


type Id
    = Id String


type CountryId
    = CountryId String


type alias Polygon =
    List Point


type alias Segment =
    ( Point, Point )


type Country
    = CoastalCountry CountryProperties (Set.Set Segment)
    | LandLockedCountry CountryProperties


type alias CountryProperties =
    { id : CountryId
    , polygon : Polygon
    , points : Set.Set Point
    }


type alias Point =
    ( Int, Int )



-- { x : Int
-- , y : Int
-- }


type alias NeighboringCountries =
    ( String, String )


type Water
    = Water (Set.Set String) -- Country Ids


type alias NewMap =
    { name : String
    , countries : List Country
    , neighboringCountries : List NeighboringCountries
    , water : List Water
    , dimensions :
        { width : Int
        , height : Int
        }
    }



-- type alias Map =
--     { id : String
--     , name : String
--     , countries : List Country
--     , neighboringCountries : List NeighboringCountries
--     , bodiesOfWater : List Water -- Could store on the CoastalProperties too. Not sure what's better.
--     }


type alias Map =
    { id : String
    , name : String
    , mapJson : String
    }


type alias NewMapRequest =
    { name : String
    , mapJson : String
    , dimensions :
        { width : Int
        , height : Int
        }
    }


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))


create : NewMap -> (RemoteData.RemoteData (Graphql.Http.Error Map) Map -> msg) -> Cmd msg
create newMap toMsg =
    let
        input =
            { map =
                { name = newMap.name
                , mapJson = newMap |> newMapToMapJson |> Json.Encode.encode 2
                , rawMap = ""
                }
            }

        mapSelection =
            Graphql.SelectionSet.map3 Map
                Api.Object.Map.id
                Api.Object.Map.name
                Api.Object.Map.mapJson
    in
    Api.Mutation.createMap input mapSelection
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


newMapToRequest : NewMap -> NewMapRequest
newMapToRequest newMap =
    { name = newMap.name
    , dimensions = newMap.dimensions
    , mapJson = newMap |> newMapToMapJson |> Json.Encode.encode 2
    }


newMapToMapJson : NewMap -> Json.Encode.Value
newMapToMapJson newMap =
    Json.Encode.object
        [ ( "name", newMap.name |> Json.Encode.string )
        , ( "dimensions"
          , Json.Encode.object
                [ ( "width", newMap.dimensions.width |> Json.Encode.int )
                , ( "height", newMap.dimensions.height |> Json.Encode.int )
                ]
          )
        , ( "countries", newMap.countries |> Json.Encode.list encodeCountry )
        ]


encodeCountry : Country -> Json.Encode.Value
encodeCountry country =
    case country of
        CoastalCountry countryProperties segments ->
            Json.Encode.object []

        LandLockedCountry countryProperties ->
            Json.Encode.object []


type alias CountryWhileParsing =
    { points : Set.Set Point
    , waterEdges : Set.Set ( Point, Point )
    }


type alias NewMapWhileParsing =
    { countries : Dict.Dict String CountryWhileParsing
    , neighboringCountries : Set.Set NeighboringCountries
    , water : Dict.Dict String (Set.Set String)
    }


type alias RawGameMap =
    Dict.Dict ( Int, Int ) String


parse : String -> String -> NewMap
parse name text =
    let
        map : RawGameMap
        map =
            GameMap.parseRawMap text

        dimensions : ( Int, Int )
        dimensions =
            getMapDimensions map

        countriesAndWater : NewMapWhileParsing
        countriesAndWater =
            map
                |> Dict.foldl
                    (\coordinates areaId newMap ->
                        if isCountry areaId then
                            newMap |> updateCountryWhileParsing areaId coordinates map

                        else
                            newMap
                    )
                    { countries = Dict.empty
                    , neighboringCountries = Set.empty
                    , water = Dict.empty
                    }
    in
    { dimensions = { width = dimensions |> Tuple.first, height = dimensions |> Tuple.second }
    , name = name
    , water = countriesAndWater.water |> Dict.values |> List.map Water
    , neighboringCountries = countriesAndWater.neighboringCountries |> Set.toList
    , countries = countriesAndWater.countries |> countriesWhileParsingToCountries
    }


countriesWhileParsingToCountries : Dict.Dict String CountryWhileParsing -> List Country
countriesWhileParsingToCountries countriesWhileParsing =
    countriesWhileParsing
        |> Dict.map
            (\countryId countryWhileParsing ->
                let
                    countryProperties : CountryProperties
                    countryProperties =
                        { polygon =
                            countryWhileParsing.points |> getSegmentsFromPoints |> segmentsToPolygon
                        , points = countryWhileParsing.points
                        , id = CountryId countryId
                        }
                in
                if (countryWhileParsing.waterEdges |> Set.size) > 0 then
                    CoastalCountry countryProperties countryWhileParsing.waterEdges

                else
                    LandLockedCountry countryProperties
            )
        |> Dict.values


getMapDimensions : RawGameMap -> ( Int, Int )
getMapDimensions map =
    map
        |> Dict.keys
        |> List.foldl
            (\( x, y ) ( width, height ) ->
                ( if x + 1 > width then
                    x + 1

                  else
                    width
                , if y + 1 > height then
                    y + 1

                  else
                    height
                )
            )
            ( 0, 0 )


updateCountryWhileParsing : String -> ( Int, Int ) -> RawGameMap -> NewMapWhileParsing -> NewMapWhileParsing
updateCountryWhileParsing countryId point rawMap newMapWhileParsing =
    let
        neighboringCountries =
            getNeighborCoordinates point
                |> Set.foldl
                    (\neighborCoordinate countries ->
                        case Dict.get neighborCoordinate rawMap of
                            Just neighborId ->
                                if neighborId /= countryId then
                                    addNeighbor ( countryId, neighborId ) newMapWhileParsing.neighboringCountries

                                else
                                    countries

                            Nothing ->
                                countries
                    )
                    Set.empty

        updatedCountry : CountryWhileParsing
        updatedCountry =
            case Dict.get countryId newMapWhileParsing.countries of
                Just country ->
                    { country | points = Set.insert point country.points }

                Nothing ->
                    { points = Set.singleton point, waterEdges = Set.empty }

        waterNeigborIdAndBorderSegments : List ( String, Segment )
        waterNeigborIdAndBorderSegments =
            adjacentEdges point
                |> List.filterMap
                    (\( _, borderSegment ) ->
                        rawMap
                            |> Dict.get point
                            |> Maybe.andThen
                                (\areaId ->
                                    if isCountry countryId then
                                        Nothing

                                    else
                                        Just ( areaId, borderSegment )
                                )
                    )

        countryWithAddedWaterBorderSegments : CountryWhileParsing
        countryWithAddedWaterBorderSegments =
            { updatedCountry
                | waterEdges =
                    Set.union
                        (waterNeigborIdAndBorderSegments
                            |> List.map (\( _, segment ) -> segment)
                            |> Set.fromList
                        )
                        updatedCountry.waterEdges
            }

        updatedCountries =
            newMapWhileParsing.countries
                |> Dict.insert countryId countryWithAddedWaterBorderSegments

        updatedWater : Dict.Dict String (Set.Set String)
        updatedWater =
            waterNeigborIdAndBorderSegments
                |> List.foldl
                    (\( waterId, _ ) result ->
                        let
                            maybeUpdatedCountriesBorderingWater : Maybe (Set.Set String)
                            maybeUpdatedCountriesBorderingWater =
                                Dict.get waterId newMapWhileParsing.water
                                    |> Maybe.map
                                        (\countryIds ->
                                            countryIds |> Set.insert countryId
                                        )

                            maybeUpdatedWater : Maybe (Dict.Dict String (Set.Set String))
                            maybeUpdatedWater =
                                maybeUpdatedCountriesBorderingWater
                                    |> Maybe.map
                                        (\updatedCountriesBorderingWater ->
                                            result |> Dict.insert waterId updatedCountriesBorderingWater
                                        )
                        in
                        maybeUpdatedWater |> Maybe.withDefault result
                    )
                    newMapWhileParsing.water
    in
    { newMapWhileParsing
        | neighboringCountries = Set.union neighboringCountries newMapWhileParsing.neighboringCountries
        , water = updatedWater
        , countries = updatedCountries
    }


addNeighbor : NeighboringCountries -> Set.Set NeighboringCountries -> Set.Set NeighboringCountries
addNeighbor ( countryId1, countryId2 ) neighboringCountries =
    -- ensures order so no duplications in the set
    if countryId1 < countryId2 then
        Set.insert ( countryId1, countryId2 ) neighboringCountries

    else
        Set.insert ( countryId2, countryId1 ) neighboringCountries


getMedianCoordinates : Set.Set Point -> ( Int, Int )
getMedianCoordinates area =
    area
        |> Set.foldl
            (\( x, y ) ( xs, ys ) ->
                ( x :: xs, y :: ys )
            )
            ( [], [] )
        |> Tuple.mapBoth List.sort List.sort
        |> Tuple.mapBoth
            (\xs ->
                xs
                    |> List.drop (Set.size area // 2)
                    |> List.head
                    |> Maybe.withDefault 0
            )
            (\ys ->
                ys
                    |> List.drop (Set.size area // 2)
                    |> List.head
                    |> Maybe.withDefault 0
            )


getNeighborCoordinates : ( Int, Int ) -> Set.Set ( Int, Int )
getNeighborCoordinates ( x, y ) =
    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
        |> List.foldl
            (\( xOffset, yOffset ) result ->
                let
                    neighborX =
                        x + xOffset

                    neighborY =
                        y + yOffset
                in
                Set.insert ( neighborX, neighborY ) result
            )
            Set.empty


isCountry : String -> Bool
isCountry areaId =
    String.length areaId < 4


segmentsToPolygon : Set.Set Segment -> List ( Int, Int )
segmentsToPolygon edges =
    let
        coordinateToPolygon : Set.Set Segment -> Point -> List Point -> List Point
        coordinateToPolygon borderSegments currentPoint result =
            let
                maybeSegment =
                    borderSegments
                        |> Set.filter
                            (\( point1, point2 ) -> point1 == currentPoint || point2 == currentPoint)
                        |> Set.toList
                        |> List.head
            in
            case maybeSegment of
                Just ( point1, point2 ) ->
                    let
                        remainingSegments =
                            borderSegments
                                |> Set.remove ( point1, point2 )
                    in
                    coordinateToPolygon remainingSegments
                        (if currentPoint == point1 then
                            point2

                         else
                            point1
                        )
                        (currentPoint :: result)

                Nothing ->
                    currentPoint :: result
    in
    case edges |> Set.toList of
        ( point1, point2 ) :: _ ->
            coordinateToPolygon (Set.remove ( point1, point2 ) edges) point2 []

        _ ->
            []


adjacentEdges : Point -> List ( Point, Segment )
adjacentEdges ( x, y ) =
    let
        left =
            ( x - 1, y )

        leftEdge =
            ( ( x, y ), ( x, y + 1 ) )

        right =
            ( x + 1, y )

        rightEdge =
            ( ( x + 1, y ), ( x + 1, y + 1 ) )

        above =
            ( x, y - 1 )

        aboveEdge =
            ( ( x, y ), ( x + 1, y ) )

        below =
            ( x, y + 1 )

        belowEdge =
            ( ( x, y + 1 ), ( x + 1, y + 1 ) )
    in
    [ ( left, leftEdge )
    , ( right, rightEdge )
    , ( above, aboveEdge )
    , ( below, belowEdge )
    ]


getSegmentsFromPoints : Set.Set ( Int, Int ) -> Set.Set Segment
getSegmentsFromPoints points =
    points
        |> Set.foldl
            (\coordinate result ->
                Set.union result (getEdgesForCountryForCoordinate points coordinate)
            )
            Set.empty


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Set.Set Segment
getEdgesForCountryForCoordinate allAreas point =
    adjacentEdges point
        |> List.foldl
            (\( adjacent, edge ) result ->
                if Set.member adjacent allAreas then
                    result

                else
                    Set.insert edge result
            )
            Set.empty
