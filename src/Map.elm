module Map exposing
    ( Id
    , Map
    , create
    , idToString
    , urlParser
    )

import Api.InputObject
import Api.Mutation
import Api.Object.Map
import Collage
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet exposing (SelectionSet)
import RemoteData
import Set
import Url.Parser


type Id
    = Id String


type CountryId
    = CountryId String


type Country
    = CoastalCountry Collage.Shape (Set.Set ( Point, Point ))
    | LandLockedCountry Collage.Shape


type alias Point =
    { x : Int
    , y : Int
    }


type alias NeighboringCountries =
    { country1 : CountryId
    , country2 : CountryId
    }


type Water
    = Water (Set.Set String) -- Country Ids



-- type alias NewMap =
--     { name : String
--     , countries : List Country
--     , neighboringCountries : List NeighboringCountries
--     , bodiesOfWater : List Water -- Could store on the CoastalProperties too. Not sure what's better.
--     }
-- type alias Map =
--     { id : String
--     , name : String
--     , countries : List Country
--     , neighboringCountries : List NeighboringCountries
--     , bodiesOfWater : List Water -- Could store on the CoastalProperties too. Not sure what's better.
--     }


type alias Map =
    { id : String
    , name : String
    , mapJson : String
    , rawMap : String
    }


type alias NewMap =
    { name : String
    , mapJson : String
    , rawMap : String
    }


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))


create : String -> String -> (RemoteData.RemoteData (Graphql.Http.Error Map) Map -> msg) -> Cmd msg
create name rawMap toMsg =
    let
        input =
            { map =
                { name = name
                , mapJson = ""
                , rawMap = rawMap
                }
            }

        mapSelection =
            Graphql.SelectionSet.map4 Map
                Api.Object.Map.id
                Api.Object.Map.name
                Api.Object.Map.mapJson
                Api.Object.Map.rawMap
    in
    Api.Mutation.createMap input mapSelection
        |> Graphql.Http.mutationRequest "http://localhost:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)
