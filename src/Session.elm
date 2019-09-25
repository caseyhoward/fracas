module Session exposing (Configuration, Session, WindowSize, configuration, gameSettings, init, navKey, updateGameMap, updateWindowSize)

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
    }


type alias Session =
    { windowSize : Maybe WindowSize
    , configuration : Maybe Configuration
    , neutralCountryTroopCounts : Maybe (Dict.Dict String TroopCount.TroopCount)
    , navKey : Browser.Navigation.Key
    , gameMap : Maybe GameMap.GameMap
    }


init : Browser.Navigation.Key -> Session
init key =
    { windowSize = Nothing
    , configuration = Nothing
    , neutralCountryTroopCounts = Nothing
    , gameMap = Nothing
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


updateGameMap : GameMap.GameMap -> Session -> Session
updateGameMap gameMap session =
    { session | gameMap = Just gameMap }


gameSettings : Session -> Maybe { numberOfPlayers : Int, gameMap : GameMap.GameMap, neutralCountryTroopCounts : Dict.Dict String TroopCount.TroopCount }
gameSettings session =
    case ( session.configuration, session.neutralCountryTroopCounts, session.gameMap ) of
        ( Just config, Just neutralCountryTroopCounts, Just gameMap ) ->
            Just
                { numberOfPlayers = config.numberOfPlayers
                , gameMap = gameMap
                , neutralCountryTroopCounts = neutralCountryTroopCounts
                }

        _ ->
            Nothing
