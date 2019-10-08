module Session exposing
    ( Session
    , WindowSize
    , addGame
    , init
    , navKey
    , updateWindowSize
    )

import Game
import Browser.Navigation
import Dict
import Map
import Maps.Big
import ViewHelpers


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Session =
    { windowSize : Maybe WindowSize
    , navKey : Browser.Navigation.Key

    -- , gameMaps : Dict.Dict String Map.Map
    , activeGames : Dict.Dict String Game.Game
    }


init : Browser.Navigation.Key -> Session
init key =
    { windowSize = Nothing
    , navKey = key

    -- , gameMaps =
    --     Dict.fromList
    --         [ ( "1", Map.parse Maps.Big.map ViewHelpers.pixelsPerMapSquare )
    --         ]
    , activeGames = Dict.empty
    }


navKey : Session -> Browser.Navigation.Key
navKey session =
    session.navKey


updateWindowSize : WindowSize -> Session -> Session
updateWindowSize windowSize session =
    { session | windowSize = Just windowSize }


addGame : Game.Id -> Game.Game -> Session -> Session
addGame (Game.Id id) activeGame session =
    { session | activeGames = Dict.insert id activeGame session.activeGames }
