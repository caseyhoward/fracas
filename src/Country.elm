module Country exposing
    ( Country
    , CountryProperties
    , Id(..)
    , Point
    , Polygon
    , Segment
    , coastal
    , decoder
    , encode
    , getScaledCountryPolygon
    , landlocked
    )

import Json.Decode
import Json.Encode
import Set


type Id
    = Id String


type Country
    = CoastalCountry CountryProperties (Set.Set Segment)
    | LandLockedCountry CountryProperties


type alias CountryProperties =
    { id : Id
    , polygon : Polygon
    , points : Set.Set Point
    }


type alias Polygon =
    List Point


type alias Segment =
    ( Point, Point )


type alias Point =
    ( Int, Int )



---- CONSTRUCTORS ----


landlocked : CountryProperties -> Country
landlocked countryProperties =
    LandLockedCountry countryProperties


coastal : CountryProperties -> Set.Set Segment -> Country
coastal countryProperties waterEdges =
    CoastalCountry countryProperties waterEdges



---- DECODER ----


decoder : Json.Decode.Decoder Country
decoder =
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
        (Json.Decode.field "id" (Json.Decode.map Id Json.Decode.string))
        (Json.Decode.field "polygon" decodePolygon)
        (Json.Decode.field "points" decodePoints)
        (Json.Decode.field "waterEdges" decodeWaterEdges)


decodePolygon : Json.Decode.Decoder Polygon
decodePolygon =
    Json.Decode.list decodePoint


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


decodeWaterEdges : Json.Decode.Decoder (Set.Set Segment)
decodeWaterEdges =
    Json.Decode.list decodeSegment
        |> Json.Decode.map Set.fromList


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



---- ENCODE ----


encode : Country -> Json.Encode.Value
encode country =
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


encodeCountryId : Id -> Json.Encode.Value
encodeCountryId (Id countryId) =
    Json.Encode.string countryId


encodePoint : Point -> Json.Encode.Value
encodePoint point =
    Json.Encode.list Json.Encode.int [ point |> Tuple.first, point |> Tuple.second ]


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


scalePolygon : Float -> Polygon -> List ( Float, Float )
scalePolygon scale polygon =
    polygon |> List.map (scalePoint scale)


scalePoint : Float -> Point -> ( Float, Float )
scalePoint scale ( x, y ) =
    ( toFloat x * scale, toFloat y * scale )


getCountryPolygon : Country -> Polygon
getCountryPolygon country =
    case country of
        LandLockedCountry countryProperties ->
            countryProperties.polygon

        CoastalCountry countryProperties _ ->
            countryProperties.polygon


getScaledCountryPolygon : Float -> Country -> List ( Float, Float )
getScaledCountryPolygon scale country =
    country |> getCountryPolygon |> scalePolygon scale
