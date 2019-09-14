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


type alias PlayerCountry =
    { countryId : String
    , population : Int
    }


numberOfPlayers : Int
numberOfPlayers =
    4


troopsPerCountry : Int
troopsPerCountry =
    3


defaultScale : Int
defaultScale =
    15


type alias Model =
    { currentPlayerTurn : PlayerTurn
    , map : GameMap
    , players : Dict.Dict Int Player
    , error : Maybe String
    }



-- type GameStage = PlacingCapitals {}


type PlayerTurn
    = PlayerTurn Int PlayerTurnStage


type PlayerTurnStage
    = CapitolPlacement
    | TroopPlacement
    | AttackAnnexOrPort
    | TroopMovement


type alias Player =
    { name : String
    , countries : Dict.Dict String PlayerCountry
    , capitolId : Maybe String
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
    ( { map = parseMap Maps.Big.map

      --   , map = parseMap mapFile |> playMap
      , players =
            List.range 1 numberOfPlayers
                |> List.map
                    (\playerId ->
                        ( playerId
                        , { countries = Dict.empty
                          , name = "Player " ++ String.fromInt playerId
                          , capitolId = Nothing
                          }
                        )
                    )
                |> Dict.fromList
      , currentPlayerTurn = PlayerTurn 1 CapitolPlacement
      , error = Nothing
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
                    case model.currentPlayerTurn of
                        PlayerTurn playerId playerTurnStage ->
                            case Dict.get playerId model.players of
                                Just currentPlayer ->
                                    ( handleCountryClickFromPlayer id country model
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleCountryClickFromPlayer : String -> Country -> Model -> Model
handleCountryClickFromPlayer countryId country model =
    case model.currentPlayerTurn of
        PlayerTurn playerId playerTurnStage ->
            case Dict.get playerId model.players of
                Just currentPlayer ->
                    case playerTurnStage of
                        CapitolPlacement ->
                            case getCountryStatus countryId currentPlayer model.players of
                                OccupiedByCurrentPlayer _ ->
                                    { model | error = Just "Error: Somehow you are placing a second capitol" }

                                OccupiedByOpponent _ _ ->
                                    { model | error = Just "You must select an unoccuppied country" }

                                Unoccupied ->
                                    let
                                        updatedPlayer =
                                            { currentPlayer
                                                | countries =
                                                    Dict.insert countryId { countryId = countryId, population = 0 } currentPlayer.countries
                                            }
                                    in
                                    { model
                                        | players = Dict.insert playerId updatedPlayer model.players
                                        , currentPlayerTurn = nextPlayerTurn model.currentPlayerTurn numberOfPlayers
                                        , error = Nothing
                                    }

                        TroopPlacement ->
                            case getCountryStatus countryId currentPlayer model.players of
                                OccupiedByCurrentPlayer playerCountry ->
                                    let
                                        updatedPlayer =
                                            { currentPlayer
                                                | countries =
                                                    Dict.insert countryId { playerCountry | population = playerCountry.population + troopsPerCountry } currentPlayer.countries
                                            }
                                    in
                                    { model
                                        | players = Dict.insert playerId updatedPlayer model.players
                                        , currentPlayerTurn = nextPlayerTurn model.currentPlayerTurn numberOfPlayers
                                        , error = Nothing
                                    }

                                OccupiedByOpponent _ _ ->
                                    { model | error = Just "You must put troops in your own country" }

                                Unoccupied ->
                                    { model | error = Just "You must put troops in your own country" }

                        AttackAnnexOrPort ->
                            case getCountryStatus countryId currentPlayer model.players of
                                OccupiedByCurrentPlayer playerCountry ->
                                    { model | error = Just "TODO: Implement attack", currentPlayerTurn = nextPlayerTurn model.currentPlayerTurn numberOfPlayers }

                                OccupiedByOpponent player playerCountry ->
                                    { model | error = Just "TODO: Implement attack", currentPlayerTurn = nextPlayerTurn model.currentPlayerTurn numberOfPlayers }

                                Unoccupied ->
                                    if canAnnexCountry model.map currentPlayer countryId then
                                        let
                                            updatedPlayer =
                                                { currentPlayer
                                                    | countries =
                                                        Dict.insert countryId { countryId = countryId, population = 0 } currentPlayer.countries
                                                }
                                        in
                                        { model
                                            | players = Dict.insert playerId updatedPlayer model.players
                                            , currentPlayerTurn = nextPlayerTurn model.currentPlayerTurn numberOfPlayers
                                            , error = Nothing
                                        }

                                    else
                                        { model
                                            | error = Just "You can't annex that country"
                                        }

                        TroopMovement ->
                            { model | error = Just "TODO: Implement troop movement", currentPlayerTurn = nextPlayerTurn model.currentPlayerTurn numberOfPlayers }

                Nothing ->
                    model



-- This should return a Result


canAnnexCountry : GameMap -> Player -> String -> Bool
canAnnexCountry gameMap player countryIdToAnnex =
    let
        playerCountries : List Country
        playerCountries =
            player.countries
                |> Dict.foldl
                    (\playerCountryId _ countries ->
                        case Dict.get playerCountryId gameMap.countries of
                            Just country ->
                                country :: countries

                            Nothing ->
                                countries
                    )
                    []

        playerNeighborCountries : List String
        playerNeighborCountries =
            playerCountries
                |> List.map
                    (\country ->
                        country.neighboringCountries |> Set.toList
                    )
                |> List.concat
    in
    List.member countryIdToAnnex playerNeighborCountries


getCountryStatus : String -> Player -> Dict.Dict Int Player -> CountryStatus
getCountryStatus countryId currentPlayer players =
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


nextPlayerTurn : PlayerTurn -> Int -> PlayerTurn
nextPlayerTurn (PlayerTurn currentPlayer playerTurnStage) totalPlayers =
    case playerTurnStage of
        TroopPlacement ->
            PlayerTurn currentPlayer AttackAnnexOrPort

        AttackAnnexOrPort ->
            PlayerTurn currentPlayer TroopMovement

        TroopMovement ->
            PlayerTurn (remainderBy totalPlayers currentPlayer + 1) TroopPlacement

        CapitolPlacement ->
            if currentPlayer == totalPlayers then
                PlayerTurn (remainderBy totalPlayers currentPlayer + 1) TroopPlacement

            else
                PlayerTurn (remainderBy totalPlayers currentPlayer + 1) CapitolPlacement


getCurrentPlayer : PlayerTurn -> Int
getCurrentPlayer (PlayerTurn currentPlayer _) =
    currentPlayer


type CountryStatus
    = Unoccupied
    | OccupiedByOpponent Player PlayerCountry
    | OccupiedByCurrentPlayer PlayerCountry



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [ Element.width Element.fill ]
        (Element.column
            [ Element.width Element.fill, Element.centerX ]
            ([ Element.el [ Element.centerX ]
                (renderMap model.players model.map
                    |> Element.html
                )
             ]
                ++ (case model.error of
                        Just error ->
                            [ Element.text error ]

                        Nothing ->
                            []
                   )
                ++ [ playerTurnStatus model.currentPlayerTurn ]
            )
        )


playerTurnStatus : PlayerTurn -> Element.Element Msg
playerTurnStatus (PlayerTurn playerId playerTurnStage) =
    Element.text
        (case playerTurnStage of
            CapitolPlacement ->
                "Player " ++ String.fromInt playerId ++ " is placing capitol"

            TroopPlacement ->
                "Player " ++ String.fromInt playerId ++ " is placing troops"

            AttackAnnexOrPort ->
                "Player " ++ String.fromInt playerId ++ " is attacking, annexing, or building a port"

            TroopMovement ->
                "Player " ++ String.fromInt playerId ++ " is moving troops"
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
                            Just ( playerId, player, playerCountry ) ->
                                case playerId of
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


findCountryOwner : Dict.Dict Int Player -> String -> Maybe ( Int, Player, PlayerCountry )
findCountryOwner players countryId =
    players
        |> Dict.foldl
            (\playerId player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countries
                            |> Maybe.map (\playerCountry -> ( playerId, player, playerCountry ))
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

        troopCountDisplay =
            if troopCount > 0 then
                String.fromInt troopCount

            else
                ""
    in
    Collage.Text.fromString troopCountDisplay
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
