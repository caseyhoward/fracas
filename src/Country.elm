module Country exposing
    ( Country
    , Id(..)
    , Point
    , ScaledCountry
    , Segment
    , SelectionSet
    , getCountriesCollage, scaledCountries
    , getCountry
    , getCountryIds, ScaledPoint
    , selectionSet
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
import Graphql.SelectionSet
import Html
import Html.Attributes
import Json.Encode
import RemoteData
import Set
import ViewHelpers


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


type alias ScaledPoint =
    ( Float, Float )


type Id
    = Id String


getCountryIds : Dict.Dict String Country -> List Id
getCountryIds countries =
    countries
        |> Dict.keys
        |> List.map Id


getCountry : Id -> Dict.Dict String Country -> Maybe Country
getCountry (Id countryId) countries =
    Dict.get countryId countries


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



---- SELECTION SETS ----


type alias SelectionSet =
    { id : String
    , coordinates : Set.Set Point -- Only needed for making the capitol dots
    , polygon : List Point
    , waterEdges : Set.Set ( Point, Point )
    , center : Point
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


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
