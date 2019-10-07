module Session exposing
    ( Session
    , WindowSize
    , addActiveGame
    , init
    , navKey
    , updateWindowSize
    )

import ActiveGame
import Browser.Navigation
import Dict
import GameMap
import Maps.Big
import ViewHelpers


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Session =
    { windowSize : Maybe WindowSize
    , navKey : Browser.Navigation.Key

    -- , gameMaps : Dict.Dict String GameMap.GameMap
    , activeGames : Dict.Dict String ActiveGame.ActiveGame
    }


init : Browser.Navigation.Key -> Session
init key =
    { windowSize = Nothing
    , navKey = key

    -- , gameMaps =
    --     Dict.fromList
    --         [ ( "1", GameMap.parse Maps.Big.map ViewHelpers.pixelsPerMapSquare )
    --         ]
    , activeGames = Dict.empty
    }


navKey : Session -> Browser.Navigation.Key
navKey session =
    session.navKey


updateWindowSize : WindowSize -> Session -> Session
updateWindowSize windowSize session =
    { session | windowSize = Just windowSize }


addActiveGame : ActiveGame.Id -> ActiveGame.ActiveGame -> Session -> Session
addActiveGame (ActiveGame.Id id) activeGame session =
    { session | activeGames = Dict.insert id activeGame session.activeGames }
