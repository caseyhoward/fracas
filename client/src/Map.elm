module Map exposing
    (  --Id(..)
       Map
       -- , Map

    ,  RawMap
       -- , create
       -- ,  errorToString
       -- , get
       -- , getAll

    , getCountriesThatCanReachCountryThroughWater
    , getMapDimensions
    , getWaterCollage
    , isCountryNeighboringWater
    , newMapWithName
    , parse
    , parseRawMap
    , view
    )

import Collage
import Collage.Render
import Color
import Country
import Dict
import Html
import Html.Attributes
import Set


type alias Map =
    { name : String
    , countries : Country.Countries
    , bodiesOfWater : Dict.Dict String (Set.Set String)
    , dimensions : ( Int, Int )
    }


type alias RawMap =
    Dict.Dict ( Int, Int ) String


getCountriesThatCanReachCountryThroughWater : Country.Countries -> Dict.Dict String (Set.Set String) -> Country.Id -> List Country.Id
getCountriesThatCanReachCountryThroughWater countries bodiesOfWater countryId =
    let
        neighboringBodiesOfWater : Set.Set String
        neighboringBodiesOfWater =
            case Country.getCountry countryId countries of
                Just countryBeingAttacked ->
                    countryBeingAttacked.neighboringBodiesOfWater

                Nothing ->
                    Set.empty
    in
    neighboringBodiesOfWater
        |> Set.foldl
            (\bodyOfWaterId countriesNeighboringWater ->
                case Dict.get bodyOfWaterId bodiesOfWater of
                    Just countryIdsNeighboringWater ->
                        (countryIdsNeighboringWater |> Set.toList |> List.map Country.Id) ++ countriesNeighboringWater

                    _ ->
                        countriesNeighboringWater
            )
            []


isCountryNeighboringWater : Country.Id -> Country.Countries -> Maybe Bool
isCountryNeighboringWater countryId countries =
    Country.getCountry countryId countries
        |> Maybe.map
            (\country ->
                Set.size country.neighboringBodiesOfWater > 0
            )


newMapWithName : String -> Map -> Map
newMapWithName name newMap =
    { newMap | name = name }


parse : String -> String -> Map
parse name text =
    let
        map =
            parseRawMap text

        dimensions =
            getMapDimensions map

        gameMapWithoutPolygons =
            map
                |> Dict.foldl
                    (\coordinates areaId gameMap ->
                        if isCountry areaId then
                            let
                                updatedCountry =
                                    Dict.get areaId gameMap.countries
                                        |> Maybe.withDefault
                                            { neighboringCountries = Set.empty
                                            , neighboringBodiesOfWater = Set.empty
                                            , coordinates = Set.singleton coordinates
                                            , polygon = []
                                            , center = ( 0, 0 )
                                            , waterEdges = Set.empty
                                            }
                                        |> updateCountryWhileParsing areaId coordinates dimensions map
                            in
                            { gameMap | countries = Dict.insert areaId updatedCountry gameMap.countries }

                        else
                            let
                                bodyOfWaterNeighborCountries =
                                    Dict.get areaId gameMap.bodiesOfWaterNeighborCountries
                                        |> Maybe.withDefault Set.empty
                            in
                            { gameMap
                                | bodiesOfWaterNeighborCountries =
                                    gameMap.bodiesOfWaterNeighborCountries
                                        |> Dict.insert
                                            areaId
                                            (updateBodyOfWater areaId coordinates dimensions map bodyOfWaterNeighborCountries)
                            }
                    )
                    { countries = Dict.empty, bodiesOfWaterNeighborCountries = Dict.empty, dimensions = dimensions }
    in
    { countries =
        gameMapWithoutPolygons.countries
            |> Dict.map
                (\_ country ->
                    let
                        edgesWithNeigborCoordinate =
                            getEdgesForArea country.coordinates

                        edgesBorderingWater =
                            edgesWithNeigborCoordinate
                                |> Set.filter
                                    (\( neighborCoordinate, _ ) ->
                                        Dict.get neighborCoordinate map
                                            |> Maybe.map
                                                (\countryIdString ->
                                                    not (isCountry countryIdString)
                                                )
                                            |> Maybe.withDefault False
                                    )
                                |> Set.map Tuple.second
                    in
                    { country
                        | polygon = coordinatesToPolygon (edgesWithNeigborCoordinate |> Set.map Tuple.second)
                        , center = Country.getMedianCoordinates country.coordinates
                        , waterEdges = edgesBorderingWater
                    }
                )
    , bodiesOfWater =
        gameMapWithoutPolygons.bodiesOfWaterNeighborCountries
    , dimensions = gameMapWithoutPolygons.dimensions
    , name = name
    }



-- NOT EXPOSED


parseRawMap : String -> RawMap
parseRawMap text =
    let
        rowStrings : List String
        rowStrings =
            String.split "\n" text
                |> List.foldl
                    (\row result ->
                        case result of
                            ( rawMap, rowIndex ) ->
                                if rowIndex then
                                    if row /= "{Country Names}" then
                                        ( row :: rawMap
                                        , True
                                        )

                                    else
                                        ( rawMap, False )

                                else if row == "{Map}" then
                                    ( rawMap, True )

                                else
                                    ( rawMap, False )
                    )
                    ( [], False )
                |> Tuple.first

        rowsAndColumns : List (List String)
        rowsAndColumns =
            rowStrings
                |> List.foldl
                    (\row result ->
                        (String.split "." row
                            |> List.reverse
                            |> List.drop 1
                            |> List.reverse
                        )
                            :: result
                    )
                    []
    in
    rowsAndColumns
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( rowIndex, splitRow ) result ->
                splitRow
                    |> List.indexedMap Tuple.pair
                    |> List.foldl
                        (\( columnIndex, areaId ) innerResult ->
                            Dict.insert ( columnIndex, rowIndex ) areaId innerResult
                        )
                        result
            )
            Dict.empty


getMapDimensions : RawMap -> ( Int, Int )
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


updateCountryWhileParsing : String -> ( Int, Int ) -> ( Int, Int ) -> RawMap -> Country.Country -> Country.Country
updateCountryWhileParsing countryId coordinates mapDimensions rawMap country =
    let
        ( neighboringCountries, neighboringBodiesOfWater ) =
            getNeighborCoordinates coordinates mapDimensions
                |> Set.foldl
                    (\neighborCoordinate ( countries, bodiesOfWater ) ->
                        case Dict.get neighborCoordinate rawMap of
                            Just neighborId ->
                                if neighborId /= countryId then
                                    if isCountry neighborId then
                                        ( Set.insert neighborId countries, bodiesOfWater )

                                    else
                                        ( countries, Set.insert neighborId bodiesOfWater )

                                else
                                    ( countries, bodiesOfWater )

                            Nothing ->
                                ( countries, bodiesOfWater )
                    )
                    ( Set.empty, Set.empty )
    in
    { country
        | neighboringCountries =
            Set.union neighboringCountries country.neighboringCountries
        , neighboringBodiesOfWater =
            Set.union neighboringBodiesOfWater country.neighboringBodiesOfWater
        , coordinates = Set.insert coordinates country.coordinates
    }


updateBodyOfWater : String -> ( Int, Int ) -> ( Int, Int ) -> RawMap -> Set.Set String -> Set.Set String
updateBodyOfWater bodyOfWaterId coordinates mapDimensions rawMap bodyOfWaterNeighborCountries =
    let
        neighboringCountries =
            getNeighborCoordinates coordinates mapDimensions
                |> Set.foldl
                    (\neighborCoordinate countries ->
                        case Dict.get neighborCoordinate rawMap of
                            Just neighborId ->
                                if neighborId /= bodyOfWaterId then
                                    if isCountry neighborId then
                                        Set.insert neighborId countries

                                    else
                                        countries

                                else
                                    countries

                            Nothing ->
                                countries
                    )
                    Set.empty
    in
    Set.union neighboringCountries bodyOfWaterNeighborCountries


getNeighborCoordinates : ( Int, Int ) -> ( Int, Int ) -> Set.Set ( Int, Int )
getNeighborCoordinates ( x, y ) ( width, height ) =
    [ ( -1, 0 ), ( 1, 0 ), ( 0, -1 ), ( 0, 1 ) ]
        |> List.foldl
            (\( xOffset, yOffset ) result ->
                let
                    neighborX =
                        x + xOffset

                    neighborY =
                        y + yOffset
                in
                if neighborX >= 0 && neighborX < width && neighborY >= 0 && neighborY < height then
                    Set.insert ( neighborX, neighborY ) result

                else
                    result
            )
            Set.empty


isCountry : String -> Bool
isCountry areaId =
    String.length areaId < 4


coordinatesToPolygon : Set.Set ( Country.Point, Country.Point ) -> List Country.Point
coordinatesToPolygon edges =
    case edges |> Set.toList of
        ( point1, point2 ) :: _ ->
            recursiveStuff (Set.remove ( point1, point2 ) edges) point2 []

        _ ->
            []


recursiveStuff : Set.Set Country.Segment -> Country.Point -> List Country.Point -> List Country.Point
recursiveStuff borderSegments currentPoint result =
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
            recursiveStuff remainingSegments
                (if currentPoint == point1 then
                    point2

                 else
                    point1
                )
                (currentPoint :: result)

        Nothing ->
            currentPoint :: result


getEdgesForArea : Country.Area -> Set.Set ( ( Int, Int ), Country.Segment )
getEdgesForArea area =
    area
        |> Set.foldl
            (\coordinate result ->
                Set.union result (getEdgesForCountryForCoordinate area coordinate)
            )
            Set.empty


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Set.Set ( ( Int, Int ), Country.Segment )
getEdgesForCountryForCoordinate allAreas ( x, y ) =
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

        adjacentEdges =
            [ ( left, leftEdge )
            , ( right, rightEdge )
            , ( above, aboveEdge )
            , ( below, belowEdge )
            ]
    in
    adjacentEdges
        |> List.foldl
            (\( adjacent, edge ) result ->
                if Set.member adjacent allAreas then
                    result

                else
                    Set.insert ( adjacent, edge ) result
            )
            Set.empty


view : Int -> Country.Countries -> ( Int, Int ) -> Html.Html msg
view scale countries ( width, height ) =
    let
        scaledWidth =
            width * scale

        scaledHeight =
            height * scale
    in
    Collage.group
        [ Country.getCountriesCollage scale countries
        , getWaterCollage scale ( width, height )
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


getWaterCollage : Int -> ( Int, Int ) -> Collage.Collage msg
getWaterCollage scale ( width, height ) =
    let
        scaledWidth =
            width * scale |> toFloat

        scaledHeight =
            height * scale |> toFloat

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

        -- backgroundBorder =
        --     background
        --         |> Collage.outlined (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 8.0) (Collage.uniform Color.black))
    in
    -- Collage.group [ backgroundBorder, backgroundWater ]
    Collage.group [ backgroundWater ]
