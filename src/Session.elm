module Session exposing
    ( Configuration
    , Session
    ,  WindowSize
       -- , configuration
       -- , gameSettings

    , addActiveGame
    , init
    ,  navKey
       -- , updateGameMap

    , updateWindowSize
    )

import ActiveGame
import Browser.Navigation
import Dict
import GameMap
import Maps.Big
import TroopCount
import ViewHelpers


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Configuration =
    { numberOfPlayers : Int
    }


type alias Session =
    { windowSize : Maybe WindowSize

    -- , activeGameId : String
    -- , activeGame : Maybe ActiveGame.ActiveGame
    -- , neutralCountryTroopCounts : Maybe (Dict.Dict String TroopCount.TroopCount)
    , navKey : Browser.Navigation.Key
    , gameMaps : Dict.Dict String GameMap.GameMap
    , activeGames : Dict.Dict String ActiveGame.ActiveGame

    -- , apiUrl : String
    }


init : Browser.Navigation.Key -> Session
init key =
    { windowSize = Nothing

    -- , configuration = Nothing
    -- , neutralCountryTroopCounts = Nothing
    -- , activeGame = Nothing
    , navKey = key
    , gameMaps =
        Dict.fromList
            [ ( "1", GameMap.parse Maps.Big.map ViewHelpers.pixelsPerMapSquare )
            ]
    , activeGames = Dict.empty

    -- , apiUrl = "http://localhost:4000"
    }


navKey : Session -> Browser.Navigation.Key
navKey session =
    session.navKey



-- getActiveGame : Session -> Maybe ActiveGame.ActiveGame
-- getActiveGame session =
--     session.activeGame


updateWindowSize : WindowSize -> Session -> Session
updateWindowSize windowSize session =
    { session | windowSize = Just windowSize }


addActiveGame : ActiveGame.Id -> ActiveGame.ActiveGame -> Session -> Session
addActiveGame (ActiveGame.Id id) activeGame session =
    { session | activeGames = Dict.insert id activeGame session.activeGames }



-- updateActiveGame : ActiveGame.ActiveGame -> Session -> Session
-- updateActiveGame activeGame session =
--     { session | activeGame = Just activeGame }
-- gameSettings : Session -> Maybe { numberOfPlayers : Int, gameMap : GameMap.GameMap, neutralCountryTroopCounts : Dict.Dict String TroopCount.TroopCount }
-- gameSettings session =
--     case ( session.configuration, session.neutralCountryTroopCounts, session.gameMap ) of
--         ( Just config, Just neutralCountryTroopCounts, Just gameMap ) ->
--             Just
--                 { numberOfPlayers = config.numberOfPlayers
--                 , gameMap = gameMap
--                 , neutralCountryTroopCounts = neutralCountryTroopCounts
--                 }
--         _ ->
--             Nothing
