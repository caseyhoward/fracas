module Main exposing (BorderSegment(..), getEdgesForArea, main, parseMap)

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


type alias Area =
    { id : String
    , coordinates : Set.Set ( Int, Int )
    }


type alias Country =
    { area : Area
    }


type alias Water =
    { area : Area
    }


type alias GameMap =
    { countries : Dict.Dict String Country
    , water : List Water
    }


type alias ParsingGameMap =
    { countries : Dict.Dict String Area
    , water : Dict.Dict String Area
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


playMap : ParsingGameMap -> GameMap
playMap parsingGameMap =
    { countries =
        parsingGameMap.countries
            |> Dict.map
                (\_ area ->
                    { area = area
                    }
                )
    , water =
        parsingGameMap.water
            |> Dict.toList
            |> List.map
                (\( _, area ) ->
                    { area = area }
                )
    }


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
      , map = parseMap Maps.SuperSimple.map |> playMap

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
    | AreaClicked String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AreaClicked id ->
            case
                model.map.countries
                    |> Dict.toList
                    |> List.map (\( _, country ) -> country)
                    |> List.filter
                        (\country ->
                            country.area.id == id
                        )
                    |> List.head
            of
                Just country ->
                    case Dict.get model.currentPlayerTurn model.players of
                        Just currentPlayer ->
                            ( handleCountryClickFromPlayer country currentPlayer model
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleCountryClickFromPlayer : Country -> Player -> Model -> Model
handleCountryClickFromPlayer country currentPlayer model =
    case getCountryStatus country currentPlayer model.players of
        OccupiedByCurrentPlayer playerCountry ->
            let
                updatedPlayer =
                    { currentPlayer
                        | countries =
                            Dict.insert country.area.id { playerCountry | population = playerCountry.population + 1 } currentPlayer.countries
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
                            Dict.insert country.area.id { countryId = country.area.id, population = 1 } currentPlayer.countries
                    }
            in
            { model
                | players = Dict.insert currentPlayer.id updatedPlayer model.players
                , currentPlayerTurn = nextPlayerTurn numberOfPlayers model.currentPlayerTurn
            }


getCountryStatus : Country -> Player -> Dict.Dict Int Player -> CountryStatus
getCountryStatus country currentPlayer players =
    case Dict.get country.area.id currentPlayer.countries of
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
                                    case Dict.get country.area.id player.countries of
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


parseMap : String -> ParsingGameMap
parseMap text =
    let
        upsideDownMap : ParsingGameMap
        upsideDownMap =
            String.split "\n" text
                |> List.foldl
                    (\row result ->
                        case result of
                            ( parsingGameMap, rowIndex ) ->
                                if rowIndex /= -1 then
                                    if row /= "{Country Names}" then
                                        ( parseCountryRow row rowIndex parsingGameMap, rowIndex + 1 )

                                    else
                                        ( parsingGameMap, -1 )

                                else if row == "{Map}" then
                                    ( parsingGameMap, 0 )

                                else
                                    ( parsingGameMap, -1 )
                    )
                    ( { countries = Dict.empty, water = Dict.empty }, -1 )
                |> Tuple.first

        mapHeight : Int
        mapHeight =
            Dict.toList upsideDownMap.countries
                |> List.map
                    (\( _, area ) ->
                        Set.toList area.coordinates
                    )
                |> List.concat
                |> List.foldl
                    (\( _, y ) maxHeight ->
                        if y > maxHeight then
                            y

                        else
                            maxHeight
                    )
                    0

        countries : Dict.Dict String Area
        countries =
            Dict.toList upsideDownMap.countries
                |> List.map
                    (\( id, area ) ->
                        ( id
                        , { area
                            | coordinates =
                                area.coordinates
                                    |> Set.toList
                                    |> List.map (\( x, y ) -> ( x, mapHeight - y ))
                                    |> Set.fromList
                          }
                        )
                    )
                |> Dict.fromList

        water : Dict.Dict String Area
        water =
            Dict.toList upsideDownMap.water
                |> List.map
                    (\( id, area ) ->
                        ( id
                        , { area
                            | coordinates =
                                area.coordinates
                                    |> Set.toList
                                    |> List.map (\( x, y ) -> ( x, mapHeight - y ))
                                    |> Set.fromList
                          }
                        )
                    )
                |> Dict.fromList
    in
    { countries = countries, water = water }



-- This is dumb. Clean it up someday.


parseCountryRow : String -> Int -> ParsingGameMap -> ParsingGameMap
parseCountryRow row rowIndex parsingGameMap =
    String.split "." row
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.foldr
            (\( columnIndex, id ) result ->
                if String.length id < 4 then
                    { result
                        | countries =
                            case Dict.get id result.countries of
                                Just area ->
                                    Dict.insert id { area | coordinates = Set.insert ( columnIndex, rowIndex ) area.coordinates } result.countries

                                Nothing ->
                                    Dict.insert id { id = id, coordinates = Set.singleton ( columnIndex, rowIndex ) } result.countries
                    }

                else
                    -- Water
                    { result
                        | water =
                            case Dict.get id result.water of
                                Just area ->
                                    Dict.insert id { area | coordinates = Set.insert ( columnIndex, rowIndex ) area.coordinates } result.water

                                Nothing ->
                                    Dict.insert id { id = id, coordinates = Set.singleton ( columnIndex, rowIndex ) } result.water
                    }
            )
            parsingGameMap



-- Rendering


renderMap : Dict.Dict Int Player -> GameMap -> Html Msg
renderMap players map =
    let
        countryCollages =
            map.countries
                |> Dict.values
                |> List.map
                    (\country ->
                        case findCountryOwner players country.area.id of
                            Just ( player, playerCountry ) ->
                                case player.id of
                                    1 ->
                                        renderCountry country.area Color.lightRed playerCountry.population

                                    2 ->
                                        renderCountry country.area Color.lightPurple playerCountry.population

                                    3 ->
                                        renderCountry country.area Color.lightYellow playerCountry.population

                                    4 ->
                                        renderCountry country.area Color.lightGreen playerCountry.population

                                    5 ->
                                        renderCountry country.area Color.lightOrange playerCountry.population

                                    6 ->
                                        renderCountry country.area Color.brown playerCountry.population

                                    _ ->
                                        renderCountry country.area Color.black playerCountry.population

                            Nothing ->
                                renderCountry country.area Color.gray 0
                    )

        waterCollages =
            List.map (\bodyOfWater -> renderArea bodyOfWater.area Color.blue) map.water
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


renderCountry : Area -> Color.Color -> Int -> Collage.Collage Msg
renderCountry area color troopCount =
    Collage.group
        [ renderTroopCount area troopCount
        , renderArea area color
        ]
        |> Collage.Events.onClick (AreaClicked area.id)


renderTroopCount : Area -> Int -> Collage.Collage msg
renderTroopCount area troopCount =
    let
        ( shiftX, shiftY ) =
            area.coordinates
                |> Set.foldl
                    (\( x, y ) ( xs, ys ) ->
                        ( x :: xs, y :: ys )
                    )
                    ( [], [] )
                |> Tuple.mapBoth List.sort List.sort
                |> Tuple.mapBoth
                    (\xs ->
                        xs
                            |> List.drop (Set.size area.coordinates // 2)
                            |> List.head
                            |> Maybe.withDefault 0
                    )
                    (\ys ->
                        ys
                            |> List.drop (Set.size area.coordinates // 2)
                            |> List.head
                            |> Maybe.withDefault 0
                    )
    in
    Collage.Text.fromString (String.fromInt troopCount)
        |> Collage.Text.color Color.black
        |> Collage.Text.size Collage.Text.small
        |> Collage.rendered
        |> Collage.shift ( (toFloat shiftX + 0.5) * toFloat defaultScale, (toFloat shiftY + 0.5) * toFloat defaultScale )



-- |> Collage.shift ( 86.02409638554218, 265.21084337349396 )
-- |> Collage.shift ( 100, 100 )


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
    area.coordinates
        |> Set.foldl
            (\coordinate result ->
                result ++ getEdgesForCountryForCoordinate area.coordinates coordinate scale
            )
            []


getBlocksForArea : Area -> Int -> Color.Color -> List (Collage.Collage msg)
getBlocksForArea area scale color =
    let
        block =
            Collage.square (toFloat scale)
                |> Collage.filled (Collage.uniform color)
    in
    area.coordinates
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
