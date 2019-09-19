module GameMap exposing
    ( BorderSegment
    , Country
    , CountryId(..)
    , GameMap
    , capitolDotsCoordinates
    , getCountriesThatCanReachCountryThroughWater
    , getCountry
    , getCountryIds
    , isCountryNeighboringWater
    , parse
    , updateCountry
    )

import Dict
import Set


type alias GameMap =
    { countries : Dict.Dict String Country
    , bodiesOfWater : Dict.Dict String (Set.Set String)
    , dimensions : ( Float, Float )
    }


type CountryId
    = CountryId String


type alias Country =
    { coordinates : Set.Set ( Int, Int ) -- Only needed for making the capitol dots
    , polygon : List ( Float, Float )
    , waterEdges : Set.Set ( ( Float, Float ), ( Float, Float ) )
    , center : ( Int, Int )
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias Area =
    Set.Set ( Int, Int )


type alias RawGameMap =
    Dict.Dict ( Int, Int ) String


type alias BorderSegment =
    ( ( Float, Float ), ( Float, Float ) )


capitolDotsCoordinates : Area -> Int -> Set.Set ( Float, Float )
capitolDotsCoordinates area scale =
    area
        |> Set.map
            (\( x, y ) ->
                ( (toFloat x + 0.5) * toFloat scale, (toFloat y + 0.5) * toFloat scale )
            )


getCountriesThatCanReachCountryThroughWater : GameMap -> CountryId -> List CountryId
getCountriesThatCanReachCountryThroughWater gameMap countryId =
    let
        neighboringBodiesOfWater =
            case getCountry countryId gameMap.countries of
                Just countryBeingAttacked ->
                    countryBeingAttacked.neighboringBodiesOfWater

                Nothing ->
                    -- TODO
                    Set.empty
    in
    neighboringBodiesOfWater
        |> Set.foldl
            (\bodyOfWaterId countries ->
                case Dict.get bodyOfWaterId gameMap.bodiesOfWater of
                    Just countryIdsNeighboringWater ->
                        (countryIdsNeighboringWater |> Set.toList |> List.map CountryId) ++ countries

                    _ ->
                        countries
            )
            []


getCountry : CountryId -> Dict.Dict String Country -> Maybe Country
getCountry (CountryId countryId) countries =
    Dict.get countryId countries


getCountryIds : Dict.Dict String Country -> List CountryId
getCountryIds countries =
    countries
        |> Dict.keys
        |> List.map CountryId


isCountryNeighboringWater : CountryId -> Dict.Dict String Country -> Maybe Bool
isCountryNeighboringWater countryId countries =
    getCountry countryId countries
        |> Maybe.map
            (\country ->
                Set.size country.neighboringBodiesOfWater > 0
            )


parse : String -> Int -> GameMap
parse text scale =
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
                                country =
                                    case Dict.get areaId gameMap.countries of
                                        Just existingCountry ->
                                            existingCountry

                                        Nothing ->
                                            { neighboringCountries = Set.empty
                                            , neighboringBodiesOfWater = Set.empty
                                            , coordinates = Set.singleton coordinates
                                            , polygon = []
                                            , center = ( 0, 0 )
                                            , waterEdges = Set.empty
                                            }

                                updatedCountry =
                                    country
                                        |> updateCountryWhileParsing areaId coordinates dimensions map
                            in
                            { gameMap | countries = Dict.insert areaId updatedCountry gameMap.countries }

                        else
                            let
                                bodyOfWaterNeighborCountries =
                                    case Dict.get areaId gameMap.bodiesOfWaterNeighborCountries of
                                        Just existingBodyOfWater ->
                                            existingBodyOfWater

                                        Nothing ->
                                            Set.empty
                            in
                            { gameMap
                                | bodiesOfWaterNeighborCountries = Dict.insert areaId (updateBodyOfWater areaId coordinates dimensions map bodyOfWaterNeighborCountries) gameMap.bodiesOfWaterNeighborCountries
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
                            getEdgesForArea country.coordinates scale

                        edgesBorderingWater =
                            edgesWithNeigborCoordinate
                                |> Set.filter
                                    (\( neighborCoordinate, _ ) ->
                                        case Dict.get neighborCoordinate map of
                                            Just countryIdString ->
                                                not (isCountry countryIdString)

                                            Nothing ->
                                                -- shouldn't happen
                                                False
                                    )
                                |> Set.map Tuple.second
                    in
                    { country
                        | polygon = coordinatesToPolygon (edgesWithNeigborCoordinate |> Set.map Tuple.second)
                        , center = getMedianCoordinates country.coordinates
                        , waterEdges = edgesBorderingWater
                    }
                )
    , bodiesOfWater =
        gameMapWithoutPolygons.bodiesOfWaterNeighborCountries
    , dimensions =
        ( (gameMapWithoutPolygons.dimensions |> Tuple.first) * scale |> toFloat
        , (gameMapWithoutPolygons.dimensions |> Tuple.second) * scale |> toFloat
        )
    }


updateCountry : CountryId -> Country -> Dict.Dict String Country -> Dict.Dict String Country
updateCountry (CountryId countryId) country countries =
    Dict.insert countryId country countries



-- NOT EXPOSED


parseRawMap : String -> RawGameMap
parseRawMap text =
    let
        rowStrings : List String
        rowStrings =
            String.split "\n" text
                |> List.foldl
                    (\row result ->
                        case result of
                            ( rawGameMap, rowIndex ) ->
                                if rowIndex then
                                    if row /= "{Country Names}" then
                                        ( row :: rawGameMap
                                        , True
                                        )

                                    else
                                        ( rawGameMap, False )

                                else if row == "{Map}" then
                                    ( rawGameMap, True )

                                else
                                    ( rawGameMap, False )
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


updateCountryWhileParsing : String -> ( Int, Int ) -> ( Int, Int ) -> RawGameMap -> Country -> Country
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


updateBodyOfWater : String -> ( Int, Int ) -> ( Int, Int ) -> RawGameMap -> Set.Set String -> Set.Set String
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


getMedianCoordinates : Area -> ( Int, Int )
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


coordinatesToPolygon : Set.Set ( ( Float, Float ), ( Float, Float ) ) -> List ( Float, Float )
coordinatesToPolygon edges =
    case edges |> Set.toList of
        ( point1, point2 ) :: _ ->
            recursiveStuff (Set.remove ( point1, point2 ) edges) point2 []

        _ ->
            []


recursiveStuff : Set.Set BorderSegment -> ( Float, Float ) -> List ( Float, Float ) -> List ( Float, Float )
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


getEdgesForArea : Area -> Int -> Set.Set ( ( Int, Int ), BorderSegment )
getEdgesForArea area scale =
    area
        |> Set.foldl
            (\coordinate result ->
                Set.union result (getEdgesForCountryForCoordinate area coordinate scale)
            )
            Set.empty


scaleCoordinate : Int -> ( Int, Int ) -> ( Float, Float )
scaleCoordinate scale ( x, y ) =
    ( x * scale |> toFloat, y * scale |> toFloat )


scaleEdge : Int -> ( ( Int, Int ), ( Int, Int ) ) -> BorderSegment
scaleEdge scale ( point1, point2 ) =
    ( scaleCoordinate scale point1, scaleCoordinate scale point2 )


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Int -> Set.Set ( ( Int, Int ), BorderSegment )
getEdgesForCountryForCoordinate allAreas ( x, y ) scaleFactor =
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
                    Set.insert ( adjacent, scaleEdge scaleFactor edge ) result
            )
            Set.empty
