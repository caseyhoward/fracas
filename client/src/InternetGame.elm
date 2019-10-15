module InternetGame exposing
    ( Configuration
    , Game
    , GameOrConfiguration(..)
    , InternetGameTokens
    , JoinToken
    , PlayerToken
    , create
    , get
    , joinGame
    , joinTokenToString
    , joinTokenUrlParser
    , playerTokenToString
    , playerTokenUrlParser
    , selectionSet
    , updateColor
    , updateMap
    , updatePlayerName
    )

import Api.Mutation
import Api.Object
import Api.Object.InternetGame
import Api.Object.InternetGameConfiguration
import Api.Object.InternetGamePlayerConfiguration
import Api.Query
import Api.Union
import Api.Union.InternetGameOrConfiguration
import Colors
import Game
import Graphql.Http
import Graphql.SelectionSet
import Map
import Player
import RemoteData
import Url.Parser


type alias PlayerConfiguration =
    { id : Player.Id
    , color : Colors.Color
    , name : String
    }


type alias Configuration =
    { players : List PlayerConfiguration
    , mapId : Map.Id
    , joinToken : JoinToken
    , currentUserPlayerId : Player.Id
    }


type JoinToken
    = JoinToken String


type PlayerToken
    = PlayerToken String


type alias InternetGameTokens =
    { joinToken : JoinToken
    , playerToken : PlayerToken
    }


type GameOrConfiguration
    = InternetGame Game
    | InternetGameConfiguration Configuration


type alias Game =
    { game : Game.Game
    , currentUserPlayerId : Player.Id
    }


create : String -> (RemoteData.RemoteData (Graphql.Http.Error PlayerToken) PlayerToken -> msg) -> Cmd msg
create apiUrl toMsg =
    (Api.Mutation.createInternetGame
        |> Graphql.SelectionSet.map PlayerToken
    )
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


get : String -> PlayerToken -> (RemoteData.RemoteData (Graphql.Http.Error GameOrConfiguration) GameOrConfiguration -> msg) -> Cmd msg
get apiUrl playerToken toMsg =
    Api.Query.internetGame
        { playerToken = playerToken |> playerTokenToString }
        selectionSet
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


joinGame : String -> JoinToken -> (RemoteData.RemoteData (Graphql.Http.Error PlayerToken) PlayerToken -> msg) -> Cmd msg
joinGame apiUrl (JoinToken joinToken) toMsg =
    Api.Mutation.joinInternetGame { joinGameToken = joinToken }
        |> Graphql.SelectionSet.map PlayerToken
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


joinTokenToString : JoinToken -> String
joinTokenToString (JoinToken joinToken) =
    joinToken


updateColor : String -> PlayerToken -> Colors.Color -> (RemoteData.RemoteData (Graphql.Http.Error Bool) Bool -> msg) -> Cmd msg
updateColor apiUrl playerToken color toMsg =
    Api.Mutation.updatePlayerColorForInternetGame
        { playerToken = playerToken |> playerTokenToString, color = color }
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


updatePlayerName : String -> PlayerToken -> String -> (RemoteData.RemoteData (Graphql.Http.Error Bool) Bool -> msg) -> Cmd msg
updatePlayerName apiUrl playerToken name toMsg =
    Api.Mutation.updatePlayerNameForInternetGame
        { playerToken = playerToken |> playerTokenToString, name = name }
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


updateMap : String -> PlayerToken -> Map.Id -> (RemoteData.RemoteData (Graphql.Http.Error Configuration) Configuration -> msg) -> Cmd msg
updateMap apiUrl playerToken mapId toMsg =
    Api.Mutation.updateMapForInternetGame
        { playerToken = playerToken |> playerTokenToString, mapId = mapId |> Map.idToString }
        configurationSelectionSet1
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


configurationSelectionSet1 : Graphql.SelectionSet.SelectionSet Configuration Api.Object.InternetGameConfiguration
configurationSelectionSet1 =
    Graphql.SelectionSet.map4
        (\players mapId joinToken currentUserPlayerId ->
            { players = players
            , mapId = mapId
            , currentUserPlayerId = Player.Id currentUserPlayerId
            , joinToken = JoinToken joinToken
            }
        )
        (Api.Object.InternetGameConfiguration.players playerConfigurationSelectionSet)
        (Graphql.SelectionSet.map Map.Id Api.Object.InternetGameConfiguration.mapId)
        Api.Object.InternetGameConfiguration.joinToken
        Api.Object.InternetGameConfiguration.currentUserPlayerId


configurationSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.InternetGameConfiguration
configurationSelectionSet =
    Graphql.SelectionSet.map InternetGameConfiguration configurationSelectionSet1


playerConfigurationSelectionSet : Graphql.SelectionSet.SelectionSet PlayerConfiguration Api.Object.InternetGamePlayerConfiguration
playerConfigurationSelectionSet =
    Graphql.SelectionSet.map3
        PlayerConfiguration
        (Graphql.SelectionSet.map Player.Id Api.Object.InternetGamePlayerConfiguration.playerId)
        (Api.Object.InternetGamePlayerConfiguration.color Colors.selectionSet)
        Api.Object.InternetGamePlayerConfiguration.name


gameSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.InternetGame
gameSelectionSet =
    Graphql.SelectionSet.map2 (\game currentUserPlayerId -> InternetGame { game = game, currentUserPlayerId = Player.Id currentUserPlayerId })
        (Api.Object.InternetGame.game Game.selectionSet)
        Api.Object.InternetGame.currentUserPlayerId


selectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Union.InternetGameOrConfiguration
selectionSet =
    Api.Union.InternetGameOrConfiguration.fragments
        { onInternetGameConfiguration = configurationSelectionSet
        , onInternetGame = gameSelectionSet
        }


playerTokenToString : PlayerToken -> String
playerTokenToString (PlayerToken token) =
    token


playerTokenUrlParser : Url.Parser.Parser (PlayerToken -> a) a
playerTokenUrlParser =
    Url.Parser.custom "PLAYERTOKEN" (\playerId -> playerId |> PlayerToken |> Just)


joinTokenUrlParser : Url.Parser.Parser (JoinToken -> a) a
joinTokenUrlParser =
    Url.Parser.custom "JOINTOKEN" (\joinToken -> joinToken |> JoinToken |> Just)
