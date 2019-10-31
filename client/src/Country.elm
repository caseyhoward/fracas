module Country exposing
    ( Area
    , Countries
    , Country
    , Id(..)
    , Point
    , ScaledCountry
    , ScaledPoint
    , Segment
    , SelectionSet
    , countryInputs
    , getCountriesCollage
    , getCountry
    , getCountryIds
    , getMedianCoordinates
    , idToString
    , scaledCountries
    , selectionSet
    , selectionSetsToCountries
    )

import Api.InputObject
import Api.Object as ApiObject
import Api.Object.Country
import Api.Object.Point
import Api.Object.Segment
import Collage
import Color
import Dict
import Graphql.SelectionSet
import Set


type alias Point =
    ( Int, Int )


type alias Segment =
    ( Point, Point )


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


type alias Countries =
    Dict.Dict String Country


type alias ScaledPoint =
    ( Float, Float )


type Id
    = Id String


type alias Area =
    Set.Set ( Int, Int )


idToString : Id -> String
idToString (Id id) =
    id


getCountryIds : Dict.Dict String Country -> List Id
getCountryIds countries =
    countries
        |> Dict.keys
        |> List.map Id


getCountry : Id -> Dict.Dict String Country -> Maybe Country
getCountry (Id countryId) countries =
    Dict.get countryId countries


scaledCountries : Int -> Dict.Dict String Country -> Dict.Dict String ScaledCountry
scaledCountries scaleFactor countries =
    countries
        |> Dict.map (\_ country -> scaleCountry scaleFactor country)


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


scalePoint : Int -> Point -> ScaledPoint
scalePoint scale ( x, y ) =
    ( x * scale |> toFloat, y * scale |> toFloat )


shiftPoint : Int -> ScaledPoint -> ScaledPoint
shiftPoint scaleFactor ( x, y ) =
    ( x + (0.5 * toFloat scaleFactor), y + (0.5 * toFloat scaleFactor) )


scaleEdge : Int -> ( ( Int, Int ), ( Int, Int ) ) -> ( ScaledPoint, ScaledPoint )
scaleEdge scale ( point1, point2 ) =
    ( scalePoint scale point1, scalePoint scale point2 )


scaleCountry : Int -> Country -> ScaledCountry
scaleCountry scaleFactor country =
    { coordinates = country.coordinates |> Set.map (scalePoint scaleFactor) |> Set.map (shiftPoint scaleFactor)
    , polygon = country.polygon |> List.map (scalePoint scaleFactor)
    , waterEdges = country.waterEdges |> Set.map (scaleEdge scaleFactor)
    , center = country.center |> scalePoint scaleFactor |> shiftPoint scaleFactor
    , neighboringCountries = country.neighboringCountries
    , neighboringBodiesOfWater = country.neighboringBodiesOfWater
    }



---- GRAPHQL ----


type alias SelectionSet =
    { id : String
    , coordinates : Set.Set Point -- Only needed for making the capitol dots
    , polygon : List Point
    , waterEdges : Set.Set ( Point, Point )
    , center : Point
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


countryInputs : Dict.Dict String Country -> List Api.InputObject.CountryInput
countryInputs countries =
    countries
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


selectionSet : Graphql.SelectionSet.SelectionSet SelectionSet ApiObject.Country
selectionSet =
    Graphql.SelectionSet.map7 SelectionSet
        Api.Object.Country.id
        (Api.Object.Country.coordinates coordinatesSelectionSet |> Graphql.SelectionSet.map Set.fromList)
        (Api.Object.Country.polygon polygonSelectionSet)
        (Api.Object.Country.waterEdges segmentSelection |> Graphql.SelectionSet.map Set.fromList)
        (Api.Object.Country.center pointSelection)
        (Api.Object.Country.neighboringCountries |> Graphql.SelectionSet.map Set.fromList)
        (Api.Object.Country.neighboringBodiesOfWater |> Graphql.SelectionSet.map Set.fromList)


selectionSetsToCountries : List SelectionSet -> Countries
selectionSetsToCountries countrySelectionSets =
    let
        selectionSetToCountry : SelectionSet -> Country
        selectionSetToCountry countrySelectionSet =
            { coordinates = countrySelectionSet.coordinates
            , polygon = countrySelectionSet.polygon
            , waterEdges = countrySelectionSet.waterEdges
            , center = countrySelectionSet.center
            , neighboringCountries = countrySelectionSet.neighboringCountries
            , neighboringBodiesOfWater = countrySelectionSet.neighboringBodiesOfWater
            }
    in
    countrySelectionSets
        |> List.map
            (\countrySelectionSet ->
                ( countrySelectionSet.id, selectionSetToCountry countrySelectionSet )
            )
        |> Dict.fromList


segmentSelection : Graphql.SelectionSet.SelectionSet Segment ApiObject.Segment
segmentSelection =
    Graphql.SelectionSet.map2 Tuple.pair
        (Api.Object.Segment.point1 pointSelection)
        (Api.Object.Segment.point2 pointSelection)


pointSelection : Graphql.SelectionSet.SelectionSet Point ApiObject.Point
pointSelection =
    Graphql.SelectionSet.map2 Tuple.pair
        Api.Object.Point.x
        Api.Object.Point.y


coordinatesSelectionSet : Graphql.SelectionSet.SelectionSet Point ApiObject.Point
coordinatesSelectionSet =
    Graphql.SelectionSet.map2 Tuple.pair
        Api.Object.Point.x
        Api.Object.Point.y


polygonSelectionSet : Graphql.SelectionSet.SelectionSet Point ApiObject.Point
polygonSelectionSet =
    Graphql.SelectionSet.map2 Tuple.pair
        Api.Object.Point.x
        Api.Object.Point.y


pointToGraphql : ( Int, Int ) -> { x : Int, y : Int }
pointToGraphql ( x, y ) =
    { x = x, y = y }


segmentToGraphql : ( ( Int, Int ), ( Int, Int ) ) -> { point1 : { x : Int, y : Int }, point2 : { x : Int, y : Int } }
segmentToGraphql ( ( x1, y1 ), ( x2, y2 ) ) =
    { point1 = { x = x1, y = y1 }, point2 = { x = x2, y = y2 } }
