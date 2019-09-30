module Game exposing
    ( Game
    , Id
    , create
    , idToString
    , urlParser
    )

import Api.Mutation
import Api.Object as ApiObject
import Api.Object.Game
import Api.Object.Map
import Collage
import Color
import Graphql.Http
import Graphql.SelectionSet exposing (SelectionSet)
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
    = PlayerId String


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
    { id : String
    , name : String
    , color : Color.Color
    , capitolId : CountryId
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
    , mapName : String
    , countries : List Country
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


gameSelection : SelectionSet GameSelectionSet ApiObject.Game
gameSelection =
    Graphql.SelectionSet.map3 GameSelectionSet
        Api.Object.Game.id
        (Api.Object.Game.map Map.mapSelection)
        Api.Object.Game.gameJson


create : String -> Int -> (RemoteData.RemoteData (Graphql.Http.Error Id) Id -> msg) -> Cmd msg
create selectedMapId numberOfPlayers toMsg =
    let
        input =
            { gameConfiguration =
                { mapId = selectedMapId
                , numberOfPlayers = numberOfPlayers
                }
            }
    in
    Api.Mutation.createGame input (Api.Object.Game.id |> Graphql.SelectionSet.map Id)
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))



-- createRequest : String -> Int -> (RemoteData.RemoteData (Graphql.Http.Error Game) Game -> msg) -> Cmd msg
-- createRequest selectedMapId numberOfPlayers toMsg =
--     let
--         input =
--             { gameConfiguration =
--                 { mapId = selectedMapId
--                 , numberOfPlayers = numberOfPlayers
--                 }
--             }
--     in
--     Api.Mutation.createGame input gameSelection
--         |> Graphql.Http.mutationRequest "http://localhost:4000"
--         |> Graphql.Http.send (RemoteData.fromResult >> toMsg)
-- mapWithJsonToMap =
