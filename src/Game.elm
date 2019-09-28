module Game exposing (Game, create)

import Collage
import Color
import Set
import TroopCount


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


create selectedMapId numberOfPlayers =
    Debug.todo "Game.create"
