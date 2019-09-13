module Main exposing (BorderSegment(..), Country, getEdgesForArea, getMapDimensions, getNeighborCoordinates, main, parseMap, parseRawMap, updateCountry)

import Browser
import Collage
import Collage.Events
import Collage.Render
import Collage.Text
import Color
import Dict
import Element
import Html exposing (Html)
import Maps.Big
import Maps.SuperSimple
import Set



---- MODEL ----
-- type alias Area =
--     { id : String
--     , coordinates : Set.Set ( Int, Int )
--     -- , neighborIds : Set.Set String
--     -- , neighborCountries : Set.Set String
--     -- , neighborBodiesOfWater : Set.Set String
--     }
-- type alias Country =
--     { area : Area
--     }
-- type alias Water =
--     { area : Area
--     }
-- type alias GameMap =
--     { countries : Dict.Dict String Country
--     , water : List Water
--     }


type alias Area =
    Set.Set ( Int, Int )


type alias GameMap =
    { countries : Dict.Dict String Country
    , bodiesOfWater : Dict.Dict String BodyOfWater
    }


type alias Country =
    { coordinates : Set.Set ( Int, Int )
    , neighboringCountries : Set.Set String
    , neighboringBodiesOfWater : Set.Set String
    }


type alias BodyOfWater =
    { coordinates : Set.Set ( Int, Int )
    , neighboringCountries : Set.Set String
    }


type alias RawGameMap =
    Dict.Dict ( Int, Int ) String


type alias ParsingArea =
    { id : String
    , coordinates : Set.Set ( Int, Int )

    -- , neighborIds : Set.Set String
    }


type alias PlayerCountry =
    { countryId : String
    , population : Int
    }


numberOfPlayers : Int
numberOfPlayers =
    6


troopsPerCountry : Int
troopsPerCountry =
    3


defaultScale : Int
defaultScale =
    15


type alias Model =
    { lastClickedAreaId : String
    , currentPlayerTurn : Int
    , map : GameMap
    , players : Dict.Dict Int Player
    }


type alias Player =
    { id : Int
    , countries : Dict.Dict String PlayerCountry
    }



-- playMap : ParsedGameMap -> GameMap
-- playMap parsedGameMap =
--     { countries =
--         parsedGameMap.countries
--             |> Dict.map
--                 (\_ area ->
--                     { area = area
--                     }
--                 )
--     , water =
--         parsedGameMap.water
--             |> Dict.toList
--             |> List.map
--                 (\( _, area ) ->
--                     { area = area }
--                 )
--     }
-- parsingArea.coordinates
--     |> Set.foldl (\coordinate ->
--     )
--     Set.empty


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { lastClickedAreaId = ""
      , map = parseMap Maps.Big.map

      --   , map = parseMap mapFile |> playMap
      , players =
            List.range 1 numberOfPlayers
                |> List.map
                    (\playerId ->
                        ( playerId
                        , { id = playerId
                          , countries = Dict.empty
                          }
                        )
                    )
                |> Dict.fromList
      , currentPlayerTurn = 1
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | CountryClicked String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CountryClicked id ->
            case Dict.get id model.map.countries of
                Just country ->
                    case Dict.get model.currentPlayerTurn model.players of
                        Just currentPlayer ->
                            ( handleCountryClickFromPlayer id country currentPlayer model
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleCountryClickFromPlayer : String -> Country -> Player -> Model -> Model
handleCountryClickFromPlayer countryId country currentPlayer model =
    case getCountryStatus countryId country currentPlayer model.players of
        OccupiedByCurrentPlayer playerCountry ->
            let
                updatedPlayer =
                    { currentPlayer
                        | countries =
                            Dict.insert countryId { playerCountry | population = playerCountry.population + 1 } currentPlayer.countries
                    }
            in
            { model
                | players = Dict.insert currentPlayer.id updatedPlayer model.players
                , currentPlayerTurn = nextPlayerTurn numberOfPlayers model.currentPlayerTurn
            }

        OccupiedByOpponent player playerCountry ->
            Debug.todo ""

        Unoccupied ->
            let
                updatedPlayer =
                    { currentPlayer
                        | countries =
                            Dict.insert countryId { countryId = countryId, population = 1 } currentPlayer.countries
                    }
            in
            { model
                | players = Dict.insert currentPlayer.id updatedPlayer model.players
                , currentPlayerTurn = nextPlayerTurn numberOfPlayers model.currentPlayerTurn
            }


getCountryStatus : String -> Country -> Player -> Dict.Dict Int Player -> CountryStatus
getCountryStatus countryId country currentPlayer players =
    case Dict.get countryId currentPlayer.countries of
        Just playerCountry ->
            OccupiedByCurrentPlayer playerCountry

        Nothing ->
            case
                players
                    |> Dict.values
                    |> List.foldl
                        (\player result ->
                            case result of
                                Just _ ->
                                    result

                                Nothing ->
                                    case Dict.get countryId player.countries of
                                        Just playerCountry ->
                                            Just (OccupiedByOpponent player playerCountry)

                                        Nothing ->
                                            Nothing
                        )
                        Nothing
            of
                Just occupiedByOppenent ->
                    occupiedByOppenent

                Nothing ->
                    Unoccupied


nextPlayerTurn : Int -> Int -> Int
nextPlayerTurn totalPlayers currentPlayer =
    remainderBy totalPlayers currentPlayer + 1


type CountryStatus
    = Unoccupied
    | OccupiedByOpponent Player PlayerCountry
    | OccupiedByCurrentPlayer PlayerCountry



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [ Element.width Element.fill ]
        (Element.row
            [ Element.width Element.fill, Element.centerX ]
            [ Element.el [ Element.centerX ]
                (renderMap model.players model.map
                    |> Element.html
                )
            ]
        )


type BorderSegment
    = BorderSegment ( Float, Float ) ( Float, Float )



-- Parsing
-- It's terrible, but it works. Eventually look into using a real parser.


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
                , if y + 1 > width then
                    y + 1

                  else
                    width
                )
            )
            ( 0, 0 )


parseMap : String -> GameMap
parseMap text =
    let
        map =
            parseRawMap text

        dimensions =
            getMapDimensions map
    in
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
                                    }
                    in
                    { gameMap
                        | countries = Dict.insert areaId (updateCountry areaId country coordinates dimensions map) gameMap.countries
                    }

                else
                    let
                        bodyOfWater =
                            case Dict.get areaId gameMap.bodiesOfWater of
                                Just existingBodyOfWater ->
                                    existingBodyOfWater

                                Nothing ->
                                    { neighboringCountries = Set.empty
                                    , coordinates = Set.singleton coordinates
                                    }
                    in
                    { gameMap
                        | bodiesOfWater = Dict.insert areaId (updateBodyOfWater areaId bodyOfWater coordinates dimensions map) gameMap.bodiesOfWater
                    }
            )
            { countries = Dict.empty, bodiesOfWater = Dict.empty }


updateCountry : String -> Country -> ( Int, Int ) -> ( Int, Int ) -> RawGameMap -> Country
updateCountry countryId country coordinates mapDimensions rawMap =
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
                                Debug.log (Debug.toString neighborCoordinate) ( countries, bodiesOfWater )
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


updateBodyOfWater : String -> BodyOfWater -> ( Int, Int ) -> ( Int, Int ) -> RawGameMap -> BodyOfWater
updateBodyOfWater bodyOfWaterId bodyOfWater coordinates mapDimensions rawMap =
    let
        ( neighboringCountries, neighboringBodiesOfWater ) =
            getNeighborCoordinates coordinates mapDimensions
                |> Set.foldl
                    (\neighborCoordinate ( countries, bodiesOfWater ) ->
                        case Dict.get neighborCoordinate rawMap of
                            Just neighborId ->
                                if neighborId /= bodyOfWaterId then
                                    if isCountry neighborId then
                                        ( Set.insert neighborId countries, bodiesOfWater )

                                    else
                                        ( countries, Set.insert neighborId bodiesOfWater )

                                else
                                    ( countries, bodiesOfWater )

                            Nothing ->
                                Debug.log (Debug.toString neighborCoordinate) ( countries, bodiesOfWater )
                    )
                    ( Set.empty, Set.empty )
    in
    { bodyOfWater
        | neighboringCountries =
            Set.union neighboringCountries bodyOfWater.neighboringCountries
        , coordinates = Set.insert coordinates bodyOfWater.coordinates
    }


addNeighborsToBodyOfWater : ( Int, Int ) -> String -> ( Int, Int ) -> RawGameMap -> GameMap -> GameMap
addNeighborsToBodyOfWater coordinates bodyOfWaterId mapDimensions rawMap gameMap =
    getNeighborCoordinates coordinates mapDimensions
        |> Set.foldl
            (\neighborCoordinate innerGameMap ->
                case Dict.get neighborCoordinate rawMap of
                    Just neighborAreaId ->
                        if isCountry neighborAreaId then
                            case Dict.get neighborAreaId innerGameMap.bodiesOfWater of
                                Just country ->
                                    let
                                        newBodiesOfWater : Dict.Dict String BodyOfWater
                                        newBodiesOfWater =
                                            Dict.insert
                                                bodyOfWaterId
                                                { country
                                                    | neighboringCountries =
                                                        Set.insert neighborAreaId country.neighboringCountries
                                                }
                                                innerGameMap.bodiesOfWater
                                    in
                                    { innerGameMap
                                        | bodiesOfWater = newBodiesOfWater
                                    }

                                Nothing ->
                                    let
                                        newBodiesOfWater : Dict.Dict String BodyOfWater
                                        newBodiesOfWater =
                                            Dict.insert
                                                bodyOfWaterId
                                                { neighboringCountries = Set.singleton neighborAreaId
                                                , coordinates = Set.singleton coordinates
                                                }
                                                innerGameMap.bodiesOfWater
                                    in
                                    { innerGameMap
                                        | bodiesOfWater = newBodiesOfWater
                                    }

                        else
                            innerGameMap

                    -- There shouldn't be water neighboring water
                    Nothing ->
                        Debug.log "this shouldn't happen" innerGameMap
            )
            gameMap


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



-- let
--     upsideDownMap : ParsedGameMap
--     upsideDownMap =
--         String.split "\n" text
--             |> List.foldl
--                 (\row result ->
--                     case result of
--                         ( parsedGameMap, rowIndex ) ->
--                             if rowIndex /= -1 then
--                                 if row /= "{Country Names}" then
--                                     ( parseCountryRow row rowIndex parsedGameMap, rowIndex + 1 )
--                                 else
--                                     ( parsedGameMap, -1 )
--                             else if row == "{Map}" then
--                                 ( parsedGameMap, 0 )
--                             else
--                                 ( parsedGameMap, -1 )
--                 )
--                 ( { countries = Dict.empty, water = Dict.empty }, -1 )
--             |> Tuple.first
--     mapHeight : Int
--     mapHeight =
--         Dict.toList upsideDownMap.countries
--             |> List.map
--                 (\( _, area ) ->
--                     Set.toList area.coordinates
--                 )
--             |> List.concat
--             |> List.foldl
--                 (\( _, y ) maxHeight ->
--                     if y > maxHeight then
--                         y
--                     else
--                         maxHeight
--                 )
--                 0
--     countries : Dict.Dict String ParsingArea
--     countries =
--         Dict.toList upsideDownMap.countries
--             |> List.map
--                 (\( id, area ) ->
--                     ( id
--                     , { area
--                         | coordinates =
--                             area.coordinates
--                                 |> Set.toList
--                                 |> List.map (\( x, y ) -> ( x, mapHeight - y ))
--                                 |> Set.fromList
--                       }
--                     )
--                 )
--             |> Dict.fromList
--     water : Dict.Dict String ParsingArea
--     water =
--         Dict.toList upsideDownMap.water
--             |> List.map
--                 (\( id, area ) ->
--                     ( id
--                     , { area
--                         | coordinates =
--                             area.coordinates
--                                 |> Set.toList
--                                 |> List.map (\( x, y ) -> ( x, mapHeight - y ))
--                                 |> Set.fromList
--                       }
--                     )
--                 )
--             |> Dict.fromList
-- in
-- { countries = countries, water = water }
-- This is dumb. Clean it up someday.
-- parseCountryRow : String -> Int -> GameMap -> GameMap
-- parseCountryRow row rowIndex parsedGameMap =
--     String.split "." row
--         |> List.reverse
--         |> List.drop 1
--         |> List.reverse
--         |> List.indexedMap Tuple.pair
--         |> List.foldr
--             (\( columnIndex, id ) result ->
--                 if isCountry id then
--                     { result
--                         | countries =
--                             case Dict.get id result.countries of
--                                 Just area ->
--                                     Dict.insert id { area | coordinates = Set.insert ( columnIndex, rowIndex ) area.coordinates } result.countries
--                                 Nothing ->
--                                     Dict.insert id { id = id, coordinates = Set.singleton ( columnIndex, rowIndex ) } result.countries
--                     }
--                 else
--                     -- Water
--                     { result
--                         | water =
--                             case Dict.get id result.water of
--                                 Just area ->
--                                     Dict.insert id { area | coordinates = Set.insert ( columnIndex, rowIndex ) area.coordinates } result.water
--                                 Nothing ->
--                                     Dict.insert id { id = id, coordinates = Set.singleton ( columnIndex, rowIndex ) } result.water
--                     }
--             )
--             parsedGameMap


isCountry : String -> Bool
isCountry areaId =
    String.length areaId < 4



-- Rendering


renderMap : Dict.Dict Int Player -> GameMap -> Html Msg
renderMap players map =
    let
        countryCollages : List (Collage.Collage Msg)
        countryCollages =
            map.countries
                |> Dict.map
                    (\countryId country ->
                        case findCountryOwner players countryId of
                            Just ( player, playerCountry ) ->
                                case player.id of
                                    1 ->
                                        renderCountry countryId country.coordinates Color.lightRed playerCountry.population

                                    2 ->
                                        renderCountry countryId country.coordinates Color.lightPurple playerCountry.population

                                    3 ->
                                        renderCountry countryId country.coordinates Color.lightYellow playerCountry.population

                                    4 ->
                                        renderCountry countryId country.coordinates Color.lightGreen playerCountry.population

                                    5 ->
                                        renderCountry countryId country.coordinates Color.lightOrange playerCountry.population

                                    6 ->
                                        renderCountry countryId country.coordinates Color.brown playerCountry.population

                                    _ ->
                                        renderCountry countryId country.coordinates Color.black playerCountry.population

                            Nothing ->
                                renderCountry countryId country.coordinates Color.gray 0
                    )
                |> Dict.values

        waterCollages =
            map.bodiesOfWater
                |> Dict.values
                |> List.map
                    (\bodyOfWater -> renderArea bodyOfWater.coordinates Color.blue)
    in
    Collage.group (countryCollages ++ waterCollages)
        |> Collage.Render.svg


findCountryOwner : Dict.Dict Int Player -> String -> Maybe ( Player, PlayerCountry )
findCountryOwner players countryId =
    players
        |> Dict.values
        |> List.foldl
            (\player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countries
                            |> Maybe.map (\playerCountry -> ( player, playerCountry ))
            )
            Nothing


renderCountry : String -> Area -> Color.Color -> Int -> Collage.Collage Msg
renderCountry countryId area color troopCount =
    Collage.group
        [ renderTroopCount area troopCount
        , renderArea area color
        ]
        |> Collage.Events.onClick (CountryClicked countryId)


renderTroopCount : Area -> Int -> Collage.Collage msg
renderTroopCount area troopCount =
    let
        ( shiftX, shiftY ) =
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
    in
    Collage.Text.fromString (String.fromInt troopCount)
        |> Collage.Text.color Color.black
        |> Collage.Text.size Collage.Text.small
        |> Collage.rendered
        |> Collage.shift ( (toFloat shiftX + 0.5) * toFloat defaultScale, (toFloat shiftY + 0.5) * toFloat defaultScale )


renderArea : Area -> Color.Color -> Collage.Collage msg
renderArea area color =
    let
        segments =
            getEdgesForArea area defaultScale
                |> List.map (\(BorderSegment p1 p2) -> Collage.segment p1 p2)

        blocks =
            getBlocksForArea area defaultScale color

        borderSegments =
            List.map
                (\segment ->
                    Collage.traced Collage.defaultLineStyle segment
                )
                segments
    in
    Collage.group (borderSegments ++ blocks)


getEdgesForArea : Area -> Int -> List BorderSegment
getEdgesForArea area scale =
    area
        |> Set.foldl
            (\coordinate result ->
                result ++ getEdgesForCountryForCoordinate area coordinate scale
            )
            []


getBlocksForArea : Area -> Int -> Color.Color -> List (Collage.Collage msg)
getBlocksForArea area scale color =
    let
        block =
            Collage.square (toFloat scale)
                |> Collage.filled (Collage.uniform color)
    in
    area
        |> Set.foldl
            (\( x, y ) result ->
                (block |> Collage.shift ( (toFloat x + 0.5) * toFloat scale, (toFloat y + 0.5) * toFloat scale )) :: result
            )
            []


scaleCoordinate : Int -> ( Int, Int ) -> ( Float, Float )
scaleCoordinate scale ( x, y ) =
    ( x * scale |> toFloat, y * scale |> toFloat )


scaleEdge : Int -> ( ( Int, Int ), ( Int, Int ) ) -> BorderSegment
scaleEdge scale ( point1, point2 ) =
    BorderSegment (scaleCoordinate scale point1) (scaleCoordinate scale point2)


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Int -> List BorderSegment
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
                    scaleEdge scaleFactor edge :: result
            )
            []
