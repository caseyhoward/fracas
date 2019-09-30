module Game exposing
    ( Game
    , Id
    , create
    , get
    , idToString
    , urlParser
    )

import Api.Mutation
import Api.Object as ApiObject
import Api.Object.Game
import Api.Object.Map
import Api.Query
import Collage
import Colors
import Dict
import Graphql.Http
import Graphql.Operation
import Graphql.OptionalArgument
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Encode
import Map
import RemoteData
import Set
import TroopCount
import Url.Parser


type OwnerId
    = PlayerOwner PlayerId
    | NeutralPlayerOwner


type CountryId
    = CountryId String


type PlayerId
    = PlayerId Int


type Country
    = CoastalCountry CountryProperties CoastalProperties
    | LandLockedCountry CountryProperties


type alias Point =
    { x : Int
    , y : Int
    }


type alias CountryProperties =
    { polygon : Collage.Shape
    , ownerId : OwnerId
    , troopCount : TroopCount.TroopCount
    }


type alias CoastalProperties =
    { waterEdges : Set.Set ( Point, Point )
    , hasPort : Bool

    -- , adjacentWater : Set.Set String
    }


type alias Player =
    { id : PlayerId
    , name : String
    , color : Colors.Color
    , capitolId : Maybe CountryId
    }


type alias NeighboringCountries =
    { country1 : CountryId
    , country2 : CountryId
    }


type Water
    = Water (Set.Set String) -- Country Ids


type TurnStatus
    = PlacingCapitol
    | PlacingReinforcements
    | Action
    | MovingTroops
    | GameOver


type alias Game =
    { id : String
    , map : Map.Map

    -- , countries : List Country
    , players : List Player
    , neighboringCountries : List NeighboringCountries
    , bodiesOfWater : List Water -- Could store on the CoastalProperties too. Not sure what's better.
    , playerTurn : PlayerId
    , turnStatus : TurnStatus
    }


type Id
    = Id String


type alias GameSelectionSet =
    { id : String
    , map : Map.Map
    , gameJson : String
    }


create : String -> Int -> (RemoteData.RemoteData (Graphql.Http.Error Id) Id -> msg) -> Cmd msg
create selectedMapId numberOfPlayers toMsg =
    let
        newGameJson : GameJson
        newGameJson =
            { players =
                List.range 1 numberOfPlayers
                    |> List.map
                        (\playerId ->
                            let
                                player : Player
                                player =
                                    { id = PlayerId playerId
                                    , name = ""
                                    , color = playerId |> PlayerId |> getDefaultColor
                                    , capitolId = Nothing
                                    }
                            in
                            player
                        )
            , playerTurn = PlayerId 1
            , turnStatus = PlacingCapitol
            }

        newGameEncoded : Json.Encode.Value
        newGameEncoded =
            newGameJson |> encodeGameJson

        input =
            { gameConfiguration =
                { mapId = selectedMapId
                , gameJson = newGameEncoded |> Json.Encode.encode 0
                }
            }
    in
    Api.Mutation.createGame input (Api.Object.Game.id |> Graphql.SelectionSet.map Id)
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


get : Id -> (RemoteData.RemoteData (Graphql.Http.Error Game) Game -> msg) -> Cmd msg
get (Id id) toMsg =
    let
        gameWithJsonSelection : SelectionSet GameSelectionSet ApiObject.Game
        gameWithJsonSelection =
            Graphql.SelectionSet.map3 GameSelectionSet
                Api.Object.Game.id
                (Api.Object.Game.map Map.mapSelection)
                Api.Object.Game.gameJson

        gameSelectionSet : SelectionSet Game ApiObject.Game
        gameSelectionSet =
            gameWithJsonSelection
                |> Graphql.SelectionSet.map
                    (\gameSelection ->
                        let
                            game : Game
                            game =
                                { id = gameSelection.id
                                , map = gameSelection.map

                                -- , countries = []
                                , players = []
                                , neighboringCountries = []
                                , bodiesOfWater = []
                                , playerTurn = PlayerId 1
                                , turnStatus = PlacingCapitol
                                }
                        in
                        game
                    )

        query : SelectionSet Game Graphql.Operation.RootQuery
        query =
            Api.Query.game (\_ -> { id = Graphql.OptionalArgument.Present id }) gameSelectionSet
    in
    query
        |> Graphql.Http.queryRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))


type alias GameJson =
    { players : List Player
    , playerTurn : PlayerId
    , turnStatus : TurnStatus
    }


encodeGameJson : GameJson -> Json.Encode.Value
encodeGameJson gameJson =
    let
        encodePlayer : Player -> Json.Encode.Value
        encodePlayer player =
            Json.Encode.object
                [ ( "id", player.id |> encodePlayerId )
                , ( "color", player.color |> Colors.encode )
                ]
    in
    Json.Encode.object
        [ ( "players", gameJson.players |> Json.Encode.list encodePlayer )
        , ( "playerTurn", gameJson.playerTurn |> encodePlayerId )
        , ( "turnStatus", gameJson.turnStatus |> encodeTurnStatus )
        ]


playerColorOptions : Dict.Dict Int Colors.Color
playerColorOptions =
    Dict.fromList
        [ ( 1, Colors.darkGreen )
        , ( 3, Colors.lightGreen )
        , ( 2, Colors.lightYellow )
        , ( 5, Colors.orange )
        , ( 4, Colors.brown )
        , ( 6, Colors.lightPurple )
        ]


getDefaultColor : PlayerId -> Colors.Color
getDefaultColor (PlayerId playerId) =
    case Dict.get playerId playerColorOptions of
        Just color ->
            color

        Nothing ->
            Colors.black


encodePlayerId : PlayerId -> Json.Encode.Value
encodePlayerId (PlayerId id) =
    Json.Encode.int id


encodeId : Id -> Json.Encode.Value
encodeId (Id id) =
    Json.Encode.string id


encodeTurnStatus : TurnStatus -> Json.Encode.Value
encodeTurnStatus turnStatus =
    (case turnStatus of
        PlacingCapitol ->
            "PlacingCapitol"

        Action ->
            "Action"

        MovingTroops ->
            "MovingTroops"

        PlacingReinforcements ->
            "PlacingReinforcements"

        GameOver ->
            "GameOver"
    )
        |> Json.Encode.string
