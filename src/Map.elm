module Map exposing
    ( Dimensions
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
import Country
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



-- type CountryId
--     = CountryId String


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
    , countries : List Country.Country
    , neighboringCountries : List NeighboringCountries
    , water : List Water
    , dimensions : Dimensions
    }


type alias Map =
    { id : String
    , name : String
    , countries : List Country.Country
    , neighboringCountries : List NeighboringCountries
    , water : List Water
    , dimensions : Dimensions
    }


type alias MapJson =
    { countries : List Country.Country
    , neighboringCountries : List NeighboringCountries
    , water : List Water
    , dimensions : Dimensions
    }



-- type alias Map =
--     { id : String
--     , name : String
--     , countries : List Country.Country
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
                (Json.Decode.field "countries" (Json.Decode.list Country.decoder))
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
        , ( "countries", newMap.countries |> Json.Encode.list Country.encode )
        , ( "neighboringCountries", newMap.neighboringCountries |> Json.Encode.list encodeNeighboringCountries )
        , ( "water", newMap.water |> Json.Encode.list encodeWater )
        ]


encodeNeighboringCountries : NeighboringCountries -> Json.Encode.Value
encodeNeighboringCountries ( countryId1, countryId2 ) =
    Json.Encode.object
        [ ( "countryId1", Json.Encode.string countryId1 )
        , ( "countryId2", Json.Encode.string countryId2 )
        ]


encodeWater : Water -> Json.Encode.Value
encodeWater (Water water) =
    Json.Encode.set Json.Encode.string water



---- PARSING ----


type alias CountryWhileParsing =
    { points : Set.Set Country.Point
    , waterEdges : Set.Set ( Country.Point, Country.Point )
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


countriesWhileParsingToCountries : Dict.Dict String CountryWhileParsing -> List Country.Country
countriesWhileParsingToCountries countriesWhileParsing =
    countriesWhileParsing
        |> Dict.map
            (\countryId countryWhileParsing ->
                let
                    countryProperties : Country.CountryProperties
                    countryProperties =
                        { polygon =
                            countryWhileParsing.points |> getSegmentsFromPoints |> segmentsToPolygon
                        , points = countryWhileParsing.points
                        , id = Country.Id countryId
                        }
                in
                if (countryWhileParsing.waterEdges |> Set.size) > 0 then
                    Country.coastal countryProperties countryWhileParsing.waterEdges

                else
                    Country.landlocked countryProperties
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

        waterNeigborIdAndBorderSegments : List ( String, Country.Segment )
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


getMedianPoint : Set.Set Country.Point -> ( Int, Int )
getMedianPoint points =
    points
        |> Set.foldl
            (\( x, y ) ( xs, ys ) ->
                ( x :: xs, y :: ys )
            )
            ( [], [] )
        |> Tuple.mapBoth List.sort List.sort
        |> Tuple.mapBoth
            (\xs ->
                xs
                    |> List.drop (Set.size points // 2)
                    |> List.head
                    |> Maybe.withDefault 0
            )
            (\ys ->
                ys
                    |> List.drop (Set.size points // 2)
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


segmentsToPolygon : Set.Set Country.Segment -> List ( Int, Int )
segmentsToPolygon edges =
    let
        coordinateToPolygon : Set.Set Country.Segment -> Country.Point -> List Country.Point -> List Country.Point
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


adjacentEdges : Country.Point -> List ( Country.Point, Country.Segment )
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


getSegmentsFromPoints : Set.Set ( Int, Int ) -> Set.Set Country.Segment
getSegmentsFromPoints points =
    points
        |> Set.foldl
            (\coordinate result ->
                Set.union result (getEdgesForCountryForCoordinate points coordinate)
            )
            Set.empty


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Set.Set Country.Segment
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
-- type alias CountryOptions =
--     { id : GameMap.CountryId
--     , color : Color.Color
--     , centerText : String
--     , canBeClicked : Bool
--     , isBeingMovedFrom : Bool
--     }


view : List Country.Country -> Dimensions -> Html.Html msg
view countries dimensions =
    let
        scaledWidth =
            dimensions.width * scale

        scaledHeight =
            dimensions.height * scale
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
                ((0 * scaledWidth |> String.fromInt)
                    ++ " "
                    ++ (-1 * scaledHeight |> String.fromInt)
                    ++ " "
                    ++ (1 * scaledWidth |> String.fromInt)
                    ++ " "
                    ++ (1 * scaledHeight |> String.fromInt)
                )
            ]


getCountriesCollage : List Country.Country -> Collage.Collage msg
getCountriesCollage countries =
    countries
        |> List.map
            (\country ->
                let
                    countryPolygon =
                        country |> Country.getScaledCountryPolygon (toFloat scale) |> Collage.polygon

                    fill =
                        countryPolygon
                            |> Collage.filled (Collage.uniform Color.gray)

                    border =
                        countryPolygon
                            |> Collage.outlined
                                (Collage.solid 30.0
                                    (Collage.uniform countryBorderColor)
                                )
                in
                Collage.group [ fill, border ]
            )
        |> Collage.group


countryBorderColor : Color.Color
countryBorderColor =
    Color.rgb255 100 100 100


getWaterCollage : Dimensions -> Collage.Collage msg
getWaterCollage dimensions =
    let
        scaledWidth =
            dimensions.width * scale |> toFloat

        scaledHeight =
            dimensions.height * scale |> toFloat

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


scale : Int
scale =
    100
