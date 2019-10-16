module Session exposing
    ( Session
    , WindowSize
    , addGame
    , init
    , navKey
    , updateWindowSize
    )

import Browser.Navigation
import Dict
import LocalGame


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Session =
    { windowSize : Maybe WindowSize
    , navKey : Browser.Navigation.Key

    -- , gameMaps : Dict.Dict String Map.Map
    , activeGames : Dict.Dict String LocalGame.Game
    , apiUrl : String
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
    , apiUrl = "http://192.168.1.7:4000"
    }


navKey : Session -> Browser.Navigation.Key
navKey session =
    session.navKey


updateWindowSize : WindowSize -> Session -> Session
updateWindowSize windowSize session =
    { session | windowSize = Just windowSize }


addGame : LocalGame.Id -> LocalGame.Game -> Session -> Session
addGame (LocalGame.Id id) activeGame session =
    { session | activeGames = Dict.insert id activeGame session.activeGames }
