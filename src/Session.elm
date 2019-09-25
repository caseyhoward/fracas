module Session exposing (Configuration, Session, WindowSize, configuration, gameSettings, init, navKey, updateWindowSize)

import Browser.Navigation
import Dict
import GameMap
import TroopCount


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Configuration =
    { numberOfPlayers : Int
    , gameMap : GameMap.GameMap
    }


type alias Session =
    { windowSize : Maybe WindowSize
    , configuration : Maybe Configuration
    , neutralCountryTroopCounts : Maybe (Dict.Dict String TroopCount.TroopCount)
    , navKey : Browser.Navigation.Key
    }


init : Browser.Navigation.Key -> Session
init key =
    { windowSize = Nothing
    , configuration = Nothing
    , neutralCountryTroopCounts = Nothing
    , navKey = key
    }


navKey : Session -> Browser.Navigation.Key
navKey session =
    session.navKey


configuration : Session -> Maybe Configuration
configuration session =
    session.configuration


updateWindowSize : WindowSize -> Session -> Session
updateWindowSize windowSize session =
    { session | windowSize = Just windowSize }


gameSettings : Session -> Maybe { numberOfPlayers : Int, gameMap : GameMap.GameMap, neutralCountryTroopCounts : Dict.Dict String TroopCount.TroopCount }
gameSettings session =
    case ( session.configuration, session.neutralCountryTroopCounts ) of
        ( Just config, Just neutralCountryTroopCounts ) ->
            Just
                { numberOfPlayers = config.numberOfPlayers
                , gameMap = config.gameMap
                , neutralCountryTroopCounts = neutralCountryTroopCounts
                }

        _ ->
            Nothing



-- updateConfiguration : Configuration -> Session -> Session
-- updateConfiguration config session =
--     { session | configuration = Just config }
