module Player exposing (Id(..), Player, idToString, urlParser)

import Colors
import Country
import Url.Parser


type Id
    = Id String


type alias Player =
    { id : Id
    , name : String
    , color : Colors.Color
    , capitolId : Maybe Country.Id
    }


idToString : Id -> String
idToString (Id id) =
    id


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "PLAYERID" (\playerId -> Just (Id playerId))
