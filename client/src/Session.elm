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
    { windowSize : WindowSize
    , navKey : Browser.Navigation.Key
    , origin : String
    , activeGames : Dict.Dict String LocalGame.Game
    , apiUrl : String
    }


init : Browser.Navigation.Key -> String -> WindowSize -> Session
init key origin windowSize =
    { windowSize = windowSize
    , navKey = key
    , origin = origin
    , activeGames = Dict.empty
    , apiUrl = "http://192.168.1.7:4000"
    }


navKey : Session -> Browser.Navigation.Key
navKey session =
    session.navKey


updateWindowSize : WindowSize -> Session -> Session
updateWindowSize windowSize session =
    { session | windowSize = windowSize }


addGame : LocalGame.Id -> LocalGame.Game -> Session -> Session
addGame (LocalGame.Id id) activeGame session =
    { session | activeGames = Dict.insert id activeGame session.activeGames }
