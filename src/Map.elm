module Map exposing
    ( Country
    , Id(..)
    , Map
    , MapSelection
    , NewMap
    , create
    , getAll
    , idToString
    , mapSelection
    , mapSelectionSetToMap
    , parse
    , urlParser
    , view
    )

import Api.Mutation
import Api.Object as ApiObject
import Api.Object.Map
import Api.Query
import Collage
import Collage.Render
import Color
import Dict
import GameMap
import Graphql.Http
import Graphql.SelectionSet exposing (SelectionSet)
import Html
import Html.Attributes
import Json.Decode
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


type alias NeighboringCountries =
    ( String, String )


type Water
    = Water (Set.Set String) -- Country Ids


type alias Dimensions =
    { width : Int
    , height : Int
    }


type alias NewMap =
    { name : String
    , countries : List Country
    , neighboringCountries : List NeighboringCountries
    , water : List Water
    , dimensions : Dimensions
    }


type alias Map =
    { id : String
    , name : String
    , countries : List Country
    , neighboringCountries : List NeighboringCountries
    , water : List Water
    , dimensions : Dimensions
    }


type alias MapJson =
    { countries : List Country
    , neighboringCountries : List NeighboringCountries
    , water : List Water
    , dimensions : Dimensions
    }



-- type alias Map =
--     { id : String
--     , name : String
--     , countries : List Country
--     , neighboringCountries : List NeighboringCountries
--     , bodiesOfWater : List Water -- Could store on the CoastalProperties too. Not sure what's better.
--     }


type alias MapSelection =
    { id : String
    , name : String
    , mapJson : String
    }


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))


mapSelection : SelectionSet MapSelection ApiObject.Map
mapSelection =
    Graphql.SelectionSet.map3 MapSelection
        Api.Object.Map.id
        Api.Object.Map.name
        Api.Object.Map.mapJson


create : NewMap -> (RemoteData.RemoteData (Graphql.Http.Error MapSelection) MapSelection -> msg) -> Cmd msg
create newMap toMsg =
    let
        input =
            { map =
                { name = newMap.name
                , mapJson = newMap |> newMapToMapJson |> Json.Encode.encode 0
                }
            }
    in
    Api.Mutation.createMap input mapSelection
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


getAll : (RemoteData.RemoteData (Graphql.Http.Error (List Map)) (List Map) -> msg) -> Cmd msg
getAll toMsg =
    Api.Query.maps mapSelection
        |> Graphql.SelectionSet.mapOrFail
            (\mapSelectionsSets ->
                mapSelectionsSets |> mapSelectionSetsToMaps
            )
        |> Graphql.Http.queryRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


mapSelectionSetsToMaps : List MapSelection -> Result String (List Map)
mapSelectionSetsToMaps mapSelectionSets =
    mapSelectionSets
        |> List.foldl
            (\mapSelectionSet result ->
                result
                    |> Result.andThen
                        (\maps ->
                            mapSelectionSet
                                |> mapSelectionSetToMap
                                |> Result.map (\map -> map :: maps)
                        )
            )
            (Ok [])


mapSelectionSetToMap : MapSelection -> Result String Map
mapSelectionSetToMap mapSelectionSet =
    case decodeMapJson mapSelectionSet.mapJson of
        Ok mapJson ->
            Ok
                { id = mapSelectionSet.id
                , countries = mapJson.countries
                , name = mapSelectionSet.name
                , dimensions = mapJson.dimensions
                , neighboringCountries = mapJson.neighboringCountries
                , water = mapJson.water
                }

        Err error ->
            Err (Json.Decode.errorToString error)


decodeMapJson : String -> Result Json.Decode.Error MapJson
decodeMapJson mapJson =
    let
        decodePoint : Json.Decode.Decoder Point
        decodePoint =
            Json.Decode.andThen
                (\values ->
                    case values of
                        x :: y :: _ ->
                            Json.Decode.succeed ( x, y )

                        _ ->
                            Json.Decode.fail "Error decoding point"
                )
                (Json.Decode.list Json.Decode.int)

        decodePolygon : Json.Decode.Decoder Polygon
        decodePolygon =
            Json.Decode.list decodePoint

        decodePoints : Json.Decode.Decoder (Set.Set Point)
        decodePoints =
            Json.Decode.list decodePoint
                |> Json.Decode.map Set.fromList

        decodeSegment : Json.Decode.Decoder Segment
        decodeSegment =
            Json.Decode.list decodePoint
                |> Json.Decode.andThen
                    (\points ->
                        case points of
                            point1 :: point2 :: _ ->
                                Json.Decode.succeed ( point1, point2 )

                            _ ->
                                Json.Decode.fail "Error decoding segment"
                    )

        decodeWaterEdges : Json.Decode.Decoder (Set.Set Segment)
        decodeWaterEdges =
            Json.Decode.list decodeSegment
                |> Json.Decode.map Set.fromList

        decodeCountry : Json.Decode.Decoder Country
        decodeCountry =
            Json.Decode.map4
                (\id polygon points waterEdges ->
                    let
                        countryProperties : CountryProperties
                        countryProperties =
                            { id = id
                            , polygon = polygon
                            , points = points
                            }
                    in
                    case Set.size waterEdges of
                        0 ->
                            LandLockedCountry countryProperties

                        _ ->
                            CoastalCountry countryProperties waterEdges
                )
                (Json.Decode.field "id" (Json.Decode.map CountryId Json.Decode.string))
                (Json.Decode.field "polygon" decodePolygon)
                (Json.Decode.field "points" decodePoints)
                (Json.Decode.field "waterEdges" decodeWaterEdges)

        decodeWater : Json.Decode.Decoder Water
        decodeWater =
            Json.Decode.map
                (\countryIds ->
                    countryIds
                        |> Set.fromList
                        |> Water
                )
                (Json.Decode.list Json.Decode.string)

        decodeNeighboringCountry : Json.Decode.Decoder NeighboringCountries
        decodeNeighboringCountry =
            Json.Decode.map2
                (\countryId1 countryId2 ->
                    ( countryId1, countryId2 )
                )
                (Json.Decode.field "countryId1" Json.Decode.string)
                (Json.Decode.field "countryId2" Json.Decode.string)

        decodeDimensions : Json.Decode.Decoder Dimensions
        decodeDimensions =
            Json.Decode.map2 Dimensions
                (Json.Decode.field "width" Json.Decode.int)
                (Json.Decode.field "height" Json.Decode.int)

        mapJsonDecoder : Json.Decode.Decoder MapJson
        mapJsonDecoder =
            Json.Decode.map4 MapJson
                (Json.Decode.field "countries" (Json.Decode.list decodeCountry))
                (Json.Decode.field "neighboringCountries" (Json.Decode.list decodeNeighboringCountry))
                (Json.Decode.field "water" (Json.Decode.list decodeWater))
                (Json.Decode.field "dimensions" decodeDimensions)
    in
    mapJson |> Json.Decode.decodeString mapJsonDecoder


newMapToMapJson : NewMap -> Json.Encode.Value
newMapToMapJson newMap =
    Json.Encode.object
        [ ( "dimensions"
          , Json.Encode.object
                [ ( "width", newMap.dimensions.width |> Json.Encode.int )
                , ( "height", newMap.dimensions.height |> Json.Encode.int )
                ]
          )
        , ( "countries", newMap.countries |> Json.Encode.list encodeCountry )
        , ( "neighboringCountries", newMap.neighboringCountries |> Json.Encode.list encodeNeighboringCountries )
        , ( "water", newMap.water |> Json.Encode.list encodeWater )
        ]


encodeCountry : Country -> Json.Encode.Value
encodeCountry country =
    case country of
        CoastalCountry countryProperties waterEdges ->
            Json.Encode.object
                (encodeCountryProperties countryProperties ++ [ ( "waterEdges", waterEdges |> Json.Encode.set encodeSegment ) ])

        LandLockedCountry countryProperties ->
            Json.Encode.object
                (encodeCountryProperties countryProperties ++ [ ( "waterEdges", Set.empty |> Json.Encode.set encodeSegment ) ])


encodeCountryProperties : CountryProperties -> List ( String, Json.Encode.Value )
encodeCountryProperties countryProperties =
    [ ( "id", encodeCountryId countryProperties.id )
    , ( "polygon", encodePolygon countryProperties.polygon )
    , ( "points", Json.Encode.set encodePoint countryProperties.points )
    ]


encodeCountryId : CountryId -> Json.Encode.Value
encodeCountryId (CountryId countryId) =
    Json.Encode.string countryId


encodePoint : Point -> Json.Encode.Value
encodePoint point =
    Json.Encode.list Json.Encode.int [ point |> Tuple.first, point |> Tuple.second ]


encodeNeighboringCountries : NeighboringCountries -> Json.Encode.Value
encodeNeighboringCountries ( countryId1, countryId2 ) =
    Json.Encode.object
        [ ( "countryId1", Json.Encode.string countryId1 )
        , ( "countryId2", Json.Encode.string countryId2 )
        ]


encodePolygon : Polygon -> Json.Encode.Value
encodePolygon polygon =
    Json.Encode.list encodePoint polygon


encodeSegment : Segment -> Json.Encode.Value
encodeSegment segment =
    Json.Encode.list
        (\point ->
            Json.Encode.list Json.Encode.int [ point |> Tuple.first, point |> Tuple.second ]
        )
        [ segment |> Tuple.first, segment |> Tuple.second ]


encodeWater : Water -> Json.Encode.Value
encodeWater (Water water) =
    Json.Encode.set Json.Encode.string water



---- PARSING ----


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



---- View


view : List Country -> Dimensions -> Html.Html msg
view countries dimensions =
    let
        scaledWidth =
            dimensions.width |> scale

        scaledHeight =
            dimensions.height |> scale
    in
    Collage.group
        [ getCountriesCollage countries
        , getWaterCollage dimensions
        ]
        |> Collage.Render.svgExplicit
            [ Html.Attributes.style "width" "100%"
            , Html.Attributes.style "max-height" "100%"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "left" "0"
            , Html.Attributes.attribute "width" "0"
            , Html.Attributes.attribute
                "viewBox"
                ((0 * scaledWidth |> String.fromFloat)
                    ++ " "
                    ++ (-1 * scaledHeight |> String.fromFloat)
                    ++ " "
                    ++ (1 * scaledWidth |> String.fromFloat)
                    ++ " "
                    ++ (1 * scaledHeight |> String.fromFloat)
                )
            ]


getCountriesCollage : List Country -> Collage.Collage msg
getCountriesCollage countries =
    countries
        |> List.map
            (\country ->
                let
                    countryPolygon =
                        country |> getCountryPolygon |> scalePolygon |> Collage.polygon

                    fill =
                        countryPolygon
                            |> Collage.filled (Collage.uniform Color.gray)

                    border =
                        countryPolygon
                            |> Collage.outlined
                                (Collage.solid 30.0
                                    (Collage.uniform Color.black)
                                )
                in
                Collage.group [ fill, border ]
            )
        |> Collage.group


scalePolygon : Polygon -> List ( Float, Float )
scalePolygon points =
    points |> List.map scalePoint


scalePoint : Point -> ( Float, Float )
scalePoint ( x, y ) =
    ( x |> scale, y |> scale )


getCountryPolygon : Country -> Polygon
getCountryPolygon country =
    case country of
        LandLockedCountry countryProperties ->
            countryProperties.polygon

        CoastalCountry countryProperties _ ->
            countryProperties.polygon


getWaterCollage : Dimensions -> Collage.Collage msg
getWaterCollage dimensions =
    let
        scaledWidth =
            dimensions.width |> scale

        scaledHeight =
            dimensions.height |> scale

        background =
            Collage.polygon
                [ ( 0, 0 )
                , ( 0, scaledHeight )
                , ( scaledWidth, scaledHeight )
                , ( scaledWidth, 0.0 )
                ]

        backgroundWater =
            background
                |> Collage.filled (Collage.uniform Color.blue)

        backgroundBorder =
            background
                |> Collage.outlined (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 8.0) (Collage.uniform Color.black))
    in
    Collage.group [ backgroundBorder, backgroundWater ]


scale : Int -> Float
scale number =
    number * 100 |> toFloat
