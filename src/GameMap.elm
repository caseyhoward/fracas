module GameMap exposing
    ( BorderSegment
    , Country
    , CountryId(..)
    , GameMap
    , Id(..)
    , NewMap
    , Point
    , RawGameMap
    , ScaledCountry
    , ScaledPoint
    , create
    , errorToString
    , get
    , getAll
    , getCountriesThatCanReachCountryThroughWater
    , getCountry
    , getCountryIds
    , getMapDimensions
    , idToString
    , isCountryNeighboringWater
    , parse
    , parseRawMap
    , scaleCountry
    , scaledCountries
    , updateCountry
    , view
    )

import Api.InputObject
import Api.Mutation
import Api.Object as ApiObject
import Api.Object.BodyOfWater
import Api.Object.Country
import Api.Object.Dimensions
import Api.Object.Map
import Api.Object.Point
import Api.Object.Segment
import Api.Query
import Collage
import Collage.Render
import Color
import Dict
import Graphql.Http
import Graphql.SelectionSet exposing (SelectionSet)
import Html
import Html.Attributes
import Json.Encode
import RemoteData
import Set
import ViewHelpers


type alias GameMap =
    { id : Id
    , name : String
    , countries : Dict.Dict String Country
    , bodiesOfWater : Dict.Dict String (Set.Set String)
    , dimensions : ( Int, Int )
    }


type alias NewMap =
    { name : String
    , countries : Dict.Dict String Country
    , bodiesOfWater : Dict.Dict String (Set.Set String)
    , dimensions : ( Int, Int )
    }


type CountryId
    = CountryId String


type Id
    = Id String


type alias Country =
    { coordinates : Set.Set Point -- Only needed for making the capitol dots
    , polygon : List Point
    , waterEdges : Set.Set ( Point, Point )
    , center : Point
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias ScaledCountry =
    { coordinates : Set.Set ScaledPoint -- Only needed for making the capitol dots
    , polygon : List ScaledPoint
    , waterEdges : Set.Set ( ScaledPoint, ScaledPoint )
    , center : ScaledPoint
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias ScaledPoint =
    ( Float, Float )


type alias Area =
    Set.Set ( Int, Int )


type alias RawGameMap =
    Dict.Dict ( Int, Int ) String


type alias BorderSegment =
    ( Point, Point )


type Error
    = Error String


type alias MapSelectionSet =
    { id : String
    , name : String
    , countries : List CountrySelectionSet
    , bodiesOfWater : List ( String, Set.Set String )
    , dimensions : ( Int, Int )
    }


type alias CountrySelectionSet =
    { id : String
    , coordinates : Set.Set Point -- Only needed for making the capitol dots
    , polygon : List Point
    , waterEdges : Set.Set ( Point, Point )
    , center : Point
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias Point =
    ( Int, Int )


type alias Segment =
    ( Point, Point )


idToString : Id -> String
idToString (Id id) =
    id


getAll : (RemoteData.RemoteData (Graphql.Http.Error (List GameMap)) (List GameMap) -> msg) -> Cmd msg
getAll toMsg =
    Api.Query.maps mapSelection
        |> Graphql.Http.queryRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


mapSelectionSet : SelectionSet MapSelectionSet ApiObject.Map
mapSelectionSet =
    let
        segmentSelection : SelectionSet Segment ApiObject.Segment
        segmentSelection =
            Graphql.SelectionSet.map2 Tuple.pair
                (Api.Object.Segment.point1 pointSelection)
                (Api.Object.Segment.point2 pointSelection)

        pointSelection : SelectionSet Point ApiObject.Point
        pointSelection =
            Graphql.SelectionSet.map2 Tuple.pair
                Api.Object.Point.x
                Api.Object.Point.y

        coordinatesSelectionSet : SelectionSet Point ApiObject.Point
        coordinatesSelectionSet =
            Graphql.SelectionSet.map2 Tuple.pair
                Api.Object.Point.x
                Api.Object.Point.y

        polygonSelectionSet : SelectionSet Point ApiObject.Point
        polygonSelectionSet =
            Graphql.SelectionSet.map2 Tuple.pair
                Api.Object.Point.x
                Api.Object.Point.y

        countrySelection : SelectionSet CountrySelectionSet ApiObject.Country
        countrySelection =
            Graphql.SelectionSet.map7 CountrySelectionSet
                Api.Object.Country.id
                (Api.Object.Country.coordinates coordinatesSelectionSet |> Graphql.SelectionSet.map Set.fromList)
                (Api.Object.Country.polygon polygonSelectionSet)
                (Api.Object.Country.waterEdges segmentSelection |> Graphql.SelectionSet.map Set.fromList)
                (Api.Object.Country.center pointSelection)
                (Api.Object.Country.neighboringCountries |> Graphql.SelectionSet.map Set.fromList)
                (Api.Object.Country.neighboringBodiesOfWater |> Graphql.SelectionSet.map Set.fromList)

        bodyOfWaterSelection : SelectionSet ( String, Set.Set String ) ApiObject.BodyOfWater
        bodyOfWaterSelection =
            Graphql.SelectionSet.map2
                (\id neighboringCountries ->
                    ( id, neighboringCountries |> Set.fromList )
                )
                Api.Object.BodyOfWater.id
                Api.Object.BodyOfWater.neighboringCountries

        dimensionsSelection : SelectionSet ( Int, Int ) ApiObject.Dimensions
        dimensionsSelection =
            Graphql.SelectionSet.map2
                (\width height ->
                    ( width, height )
                )
                Api.Object.Dimensions.width
                Api.Object.Dimensions.height
    in
    Graphql.SelectionSet.map5 MapSelectionSet
        Api.Object.Map.id
        Api.Object.Map.name
        (Api.Object.Map.countries countrySelection)
        (Api.Object.Map.bodiesOfWater bodyOfWaterSelection)
        (Api.Object.Map.dimensions dimensionsSelection)


mapSelection : SelectionSet GameMap ApiObject.Map
mapSelection =
    mapSelectionSet
        |> Graphql.SelectionSet.map mapSelectionSetToMap


pointToGraphql : ( Int, Int ) -> { x : Int, y : Int }
pointToGraphql ( x, y ) =
    { x = x, y = y }


segmentToGraphql : ( ( Int, Int ), ( Int, Int ) ) -> { point1 : { x : Int, y : Int }, point2 : { x : Int, y : Int } }
segmentToGraphql ( ( x1, y1 ), ( x2, y2 ) ) =
    { point1 = { x = x1, y = y1 }, point2 = { x = x2, y = y2 } }


create : NewMap -> (RemoteData.RemoteData (Graphql.Http.Error GameMap) GameMap -> msg) -> Cmd msg
create newMap toMsg =
    let
        countryInputs : List Api.InputObject.CountryInput
        countryInputs =
            newMap.countries
                |> Dict.map
                    (\countryId country ->
                        let
                            center : Api.InputObject.PointInput
                            center =
                                country.coordinates |> getMedianCoordinates |> pointToGraphql |> Api.InputObject.buildPointInput

                            coordinates : List Api.InputObject.PointInput
                            coordinates =
                                country.coordinates |> Set.toList |> List.map pointToGraphql |> List.map Api.InputObject.buildPointInput

                            polygon : List Api.InputObject.PointInput
                            polygon =
                                country.polygon |> List.map pointToGraphql |> List.map Api.InputObject.buildPointInput

                            waterEdges : List Api.InputObject.SegmentInput
                            waterEdges =
                                country.waterEdges |> Set.toList |> List.map segmentToGraphql |> List.map Api.InputObject.buildSegmentInput

                            neighboringCountries =
                                country.neighboringCountries |> Set.toList
                        in
                        { id = countryId
                        , coordinates = coordinates
                        , polygon = polygon
                        , waterEdges = waterEdges
                        , center = center
                        , neighboringCountries = neighboringCountries
                        , neighboringBodiesOfWater = country.neighboringBodiesOfWater |> Set.toList
                        }
                            |> Api.InputObject.buildCountryInput
                    )
                |> Dict.values

        bodiesOfWater : List Api.InputObject.BodyOfWaterInput
        bodiesOfWater =
            newMap.bodiesOfWater
                |> Dict.map
                    (\waterId countries ->
                        { id = waterId, neighboringCountries = countries |> Set.toList }
                    )
                |> Dict.values

        dimensionsInput : Api.InputObject.DimensionsInput
        dimensionsInput =
            case newMap.dimensions of
                ( width, height ) ->
                    { width = width, height = height }

        requiredFields : Api.InputObject.MapInputRequiredFields
        requiredFields =
            { name = newMap.name
            , countries = countryInputs
            , bodiesOfWater = bodiesOfWater
            , dimensions = dimensionsInput
            }

        input : Api.InputObject.MapInput
        input =
            requiredFields |> Api.InputObject.buildMapInput
    in
    Api.Mutation.createMap { map = input } mapSelection
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


mapSelectionSetToMap : MapSelectionSet -> GameMap
mapSelectionSetToMap selectionSet =
    let
        countrySelectionSetsToCountries : List CountrySelectionSet -> Dict.Dict String Country
        countrySelectionSetsToCountries countrySelectionSets =
            countrySelectionSets
                |> List.map
                    (\countrySelectionSet ->
                        ( countrySelectionSet.id
                        , { coordinates = countrySelectionSet.coordinates
                          , polygon = countrySelectionSet.polygon
                          , waterEdges = countrySelectionSet.waterEdges
                          , center = countrySelectionSet.center
                          , neighboringCountries = countrySelectionSet.neighboringCountries
                          , neighboringBodiesOfWater = countrySelectionSet.neighboringBodiesOfWater
                          }
                        )
                    )
                |> Dict.fromList

        bodiesOfWaterSelectionToBodiesOfWater : List ( String, Set.Set String ) -> Dict.Dict String (Set.Set String)
        bodiesOfWaterSelectionToBodiesOfWater waterSelectionSet =
            waterSelectionSet |> Dict.fromList
    in
    { id = selectionSet.id |> Id
    , name = selectionSet.name
    , countries = selectionSet.countries |> countrySelectionSetsToCountries
    , bodiesOfWater = selectionSet.bodiesOfWater |> bodiesOfWaterSelectionToBodiesOfWater
    , dimensions = selectionSet.dimensions
    }


errorToString : Error -> String
errorToString (Error error) =
    error


get : Id -> Dict.Dict String GameMap -> Result Error GameMap
get (Id id) gameMaps =
    case Dict.get id gameMaps of
        Just gameMap ->
            Ok gameMap

        Nothing ->
            Error "Game map not found" |> Err


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


parse : String -> String -> NewMap
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
                        , center = getMedianCoordinates country.coordinates
                        , waterEdges = edgesBorderingWater
                    }
                )
    , bodiesOfWater =
        gameMapWithoutPolygons.bodiesOfWaterNeighborCountries
    , dimensions = gameMapWithoutPolygons.dimensions
    , name = name
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


coordinatesToPolygon : Set.Set ( Point, Point ) -> List Point
coordinatesToPolygon edges =
    case edges |> Set.toList of
        ( point1, point2 ) :: _ ->
            recursiveStuff (Set.remove ( point1, point2 ) edges) point2 []

        _ ->
            []


recursiveStuff : Set.Set BorderSegment -> Point -> List Point -> List Point
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


getEdgesForArea : Area -> Set.Set ( ( Int, Int ), BorderSegment )
getEdgesForArea area =
    area
        |> Set.foldl
            (\coordinate result ->
                Set.union result (getEdgesForCountryForCoordinate area coordinate)
            )
            Set.empty


scalePoint : Int -> Point -> ScaledPoint
scalePoint scale ( x, y ) =
    ( x * scale |> toFloat, y * scale |> toFloat )


scaleEdge : Int -> ( ( Int, Int ), ( Int, Int ) ) -> ( ScaledPoint, ScaledPoint )
scaleEdge scale ( point1, point2 ) =
    ( scalePoint scale point1, scalePoint scale point2 )


scaleCountry : Int -> Country -> ScaledCountry
scaleCountry scaleFactor country =
    { coordinates = country.coordinates |> Set.map (scalePoint scaleFactor)
    , polygon = country.polygon |> List.map (scalePoint scaleFactor)
    , waterEdges = country.waterEdges |> Set.map (scaleEdge scaleFactor)
    , center = country.center |> scalePoint scaleFactor
    , neighboringCountries = country.neighboringCountries
    , neighboringBodiesOfWater = country.neighboringBodiesOfWater
    }


scaledCountries : Int -> Dict.Dict String Country -> Dict.Dict String ScaledCountry
scaledCountries scaleFactor countries =
    countries
        |> Dict.map (\_ country -> scaleCountry scaleFactor country)


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Set.Set ( ( Int, Int ), BorderSegment )
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


view : Int -> Dict.Dict String Country -> ( Int, Int ) -> Html.Html msg
view scale countries ( width, height ) =
    let
        scaledWidth =
            width * scale

        scaledHeight =
            height * scale
    in
    Collage.group
        [ getCountriesCollage scale countries
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


getCountriesCollage : Int -> Dict.Dict String Country -> Collage.Collage msg
getCountriesCollage scale countries =
    countries
        |> Dict.map
            (\_ country ->
                let
                    countryPolygon =
                        country |> scaleCountry scale |> .polygon |> Collage.polygon

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
        |> Dict.values
        |> Collage.group


countryBorderColor : Color.Color
countryBorderColor =
    Color.rgb255 100 100 100


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

        backgroundBorder =
            background
                |> Collage.outlined (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 8.0) (Collage.uniform Color.black))
    in
    Collage.group [ backgroundBorder, backgroundWater ]
