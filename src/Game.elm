module Game exposing
    ( Game
    , Id
    , create
    , get
    , idToString
    , urlParser
    , view
    )

-- import Api.Query

import Api.Mutation
import Api.Object as ApiObject
import Api.Object.Game
import Api.Object.Map
import Colors
import Country
import Dict
import Element
import Graphql.Http
import Graphql.Operation
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode
import Json.Encode
import Map
import Player
import RemoteData
import Set
import TroopCount
import Url.Parser



-- type OwnerId
--     = PlayerOwner Player.Id
--     | NeutralPlayerOwner


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
    , players : List Player.Player
    , playerTurn : Player.Id
    , turnStatus : TurnStatus
    }



-- type alias GameCountry =
--     { countryId : Map.CountryId
--     , hasPort : Bool
--     , troopCount : TroopCount.TroopCount
--     , playerId : Maybe Player.Id
--     }


type Id
    = Id String


type alias GameSelectionSet =
    { id : String
    , map : Map.Map
    , gameJson : String
    }


create : String -> Int -> (RemoteData.RemoteData (Graphql.Http.Error Id) Id -> msg) -> Cmd msg
create selectedMapId numberOfPlayers toMsg =
    Debug.todo ""



-- let
--     newGameJson : GameJson
--     newGameJson =
--         { players =
--             List.range 1 numberOfPlayers
--                 |> List.map
--                     (\playerId ->
--                         let
--                             player : Player.Player
--                             player =
--                                 { id = Player.Id (String.fromInt playerId)
--                                 , name = ""
--                                 , color = String.fromInt playerId |> Player.Id |> getDefaultColor
--                                 , capitolId = Nothing
--                                 }
--                         in
--                         player
--                     )
--         , playerTurn = Player.Id "1"
--         , turnStatus = PlacingCapitol
--         }
--     newGameEncoded : Json.Encode.Value
--     newGameEncoded =
--         newGameJson |> encodeGameJson
--     input =
--         { newGame =
--             { mapId = selectedMapId
--             , gameJson = newGameEncoded |> Json.Encode.encode 0
--             }
--         }
-- in
-- Api.Mutation.createGame input (Api.Object.Game.id |> Graphql.SelectionSet.map Id)
--     |> Graphql.Http.mutationRequest "http://localhost:4000"
--     |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


get : Id -> (RemoteData.RemoteData (Graphql.Http.Error Game) Game -> msg) -> Cmd msg
get (Id id) toMsg =
    Debug.todo ""



-- let
--     c : SelectionSet Map.Map ApiObject.Map
--     c =
--         Graphql.SelectionSet.map3 Map.MapSelection
--             Api.Object.Map.id
--             Api.Object.Map.name
--             Api.Object.Map.mapJson
--             |> Graphql.SelectionSet.mapOrFail Map.mapSelectionSetToMap
--     gameSelectionSet : SelectionSet GameSelectionSet ApiObject.Game
--     gameSelectionSet =
--         Graphql.SelectionSet.map3 GameSelectionSet
--             Api.Object.Game.id
--             (Api.Object.Game.map c)
--             Api.Object.Game.gameJson
--     game1 : SelectionSet Game ApiObject.Game
--     game1 =
--         gameSelectionSet
--             |> Graphql.SelectionSet.mapOrFail
--                 (\gameSelection ->
--                     let
--                         gameJsonResult : Result Json.Decode.Error GameJson
--                         gameJsonResult =
--                             decodeGameJson gameSelection.gameJson
--                         game : Result String Game
--                         game =
--                             gameJsonResult
--                                 |> Result.map
--                                     (\gameJson ->
--                                         { id = gameSelection.id
--                                         , map = gameSelection.map
--                                         , players = gameJson.players
--                                         , playerTurn = gameJson.playerTurn
--                                         , turnStatus = gameJson.turnStatus
--                                         }
--                                     )
--                                 |> Result.mapError Json.Decode.errorToString
--                     in
--                     game
--                 )
--     query : SelectionSet Game Graphql.Operation.RootQuery
--     query =
--         Api.Query.game { id = id } game1
-- in
-- query
--     |> Graphql.Http.queryRequest "http://localhost:4000"
--     |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


view : List Country.Country -> Map.Dimensions -> Element.Element msg
view countries dimensions =
    Map.view countries dimensions |> Element.html


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))


type alias GameJson =
    { players : List Player.Player
    , playerTurn : Player.Id
    , turnStatus : TurnStatus
    }


decodeGameJson : String -> Result Json.Decode.Error GameJson
decodeGameJson json =
    Debug.todo ""



--     let
--         decodeTurnStatus : Json.Decode.Decoder TurnStatus
--         decodeTurnStatus =
--             Json.Decode.andThen
--                 (\value ->
--                     case value of
--                         "PlacingCapitol" ->
--                             Json.Decode.succeed PlacingCapitol
--                         "Action" ->
--                             Json.Decode.succeed Action
--                         "MovingTroops" ->
--                             Json.Decode.succeed MovingTroops
--                         "PlacingReinforcements" ->
--                             Json.Decode.succeed PlacingReinforcements
--                         "GameOver" ->
--                             Json.Decode.succeed GameOver
--                         _ ->
--                             Json.Decode.fail "Unknown TurnStatus"
--                 )
--                 Json.Decode.string
--         decodePlayer : Json.Decode.Decoder Player.Player
--         decodePlayer =
--             Json.Decode.map4 Player.Player
--                 (Json.Decode.field "id" Json.Decode.string |> Json.Decode.map Player.Id)
--                 (Json.Decode.field "name" Json.Decode.string)
--                 (Json.Decode.field "color" Colors.decoder)
--                 (Json.Decode.field "capitolId" (Json.Decode.string |> Json.Decode.map Country.Id |> Json.Decode.nullable))
--         decoder : Json.Decode.Decoder GameJson
--         decoder =
--             Json.Decode.map3 GameJson
--                 (Json.Decode.field "players" (Json.Decode.list decodePlayer))
--                 (Json.Decode.field "playerTurn" (Json.Decode.string |> Json.Decode.map Player.Id))
--                 (Json.Decode.field "turnStatus" decodeTurnStatus)
--     in
--     Json.Decode.decodeString decoder json


encodeGameJson : GameJson -> Json.Encode.Value
encodeGameJson gameJson =
    Debug.todo ""



-- let
--     encodePlayer : Player.Player -> Json.Encode.Value
--     encodePlayer player =
--         Json.Encode.object
--             ([ ( "id", player.id |> encodePlayerId )
--              , ( "name", player.name |> Json.Encode.string )
--              , ( "color", player.color |> Colors.encode )
--              ]
--                 ++ (case player.capitolId of
--                         Just capitolId ->
--                             [ ( "capitolId", capitolId |> encodeCountryId ) ]
--                         Nothing ->
--                             [ ( "capitolId", Json.Encode.null ) ]
--                    )
--             )
-- in
-- Json.Encode.object
--     [ ( "players", gameJson.players |> Json.Encode.list encodePlayer )
--     , ( "playerTurn", gameJson.playerTurn |> encodePlayerId )
--     , ( "turnStatus", gameJson.turnStatus |> encodeTurnStatus )
--     ]


encodeCountryId : Country.Id -> Json.Encode.Value
encodeCountryId (Country.Id countryId) =
    Json.Encode.string countryId


playerColorOptions : Dict.Dict String Colors.Color
playerColorOptions =
    Dict.fromList
        [ ( "1", Colors.darkGreen )
        , ( "3", Colors.lightGreen )
        , ( "2", Colors.lightYellow )
        , ( "5", Colors.orange )
        , ( "4", Colors.brown )
        , ( "6", Colors.lightPurple )
        ]


getDefaultColor : Player.Id -> Colors.Color
getDefaultColor (Player.Id playerId) =
    case Dict.get playerId playerColorOptions of
        Just color ->
            color

        Nothing ->
            Colors.black


encodePlayerId : Player.Id -> Json.Encode.Value
encodePlayerId (Player.Id id) =
    Json.Encode.string id



-- encodeId : Id -> Json.Encode.Value
-- encodeId (Id id) =
--     Json.Encode.string id


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
