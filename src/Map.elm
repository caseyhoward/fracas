module Map exposing
    ( Id
    , Map
    , idToString
    , urlParser
    )

import Collage
import Set
import Url.Parser


type Id
    = Id String


type CountryId
    = CountryId String


type Country
    = CoastalCountry CountryProperties CoastalProperties
    | LandLockedCountry CountryProperties


type alias Point =
    { x : Int
    , y : Int
    }


type alias CountryProperties =
    { polygon : Collage.Shape
    }


type alias CoastalProperties =
    { waterEdges : Set.Set ( Point, Point )
    , hasPort : Bool

    -- , adjacentWater : Set.Set String
    }


type alias NeighboringCountries =
    { country1 : CountryId
    , country2 : CountryId
    }


type Water
    = Water (Set.Set String) -- Country Ids


type alias Map =
    { id : String
    , name : String
    , countries : List Country
    , neighboringCountries : List NeighboringCountries
    , bodiesOfWater : List Water -- Could store on the CoastalProperties too. Not sure what's better.
    }


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))
