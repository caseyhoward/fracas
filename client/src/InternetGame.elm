module InternetGame exposing
    ( Configuration
    , GameOrConfiguration(..)
    , InternetGameTokens
    , JoinToken
    , PlayerToken
    , create
    , get
    , joinGame
    , joinTokenToString
    , playerTokenToString
    , playerTokenUrlParser
    , selectionSet
    , updateMap, joinTokenUrlParser
    )

import Api.Mutation
import Api.Object
import Api.Object.InternetGameConfiguration
import Api.Object.InternetGamePlayerConfiguration
import Api.Query
import Api.Union
import Api.Union.InternetGame
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
    , userPlayerId : Player.Id
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
    = InternetGame Game.Game
    | InternetGameConfiguration Configuration


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


updateMap : String -> PlayerToken -> Map.Id -> (RemoteData.RemoteData (Graphql.Http.Error GameOrConfiguration) GameOrConfiguration -> msg) -> Cmd msg
updateMap apiUrl playerToken mapId toMsg =
    Api.Mutation.updateMapForInternetGame
        { playerToken = playerToken |> playerTokenToString, mapId = mapId |> Map.idToString }
        selectionSet
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


configurationSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.InternetGameConfiguration
configurationSelectionSet =
    Graphql.SelectionSet.map4
        (\players mapId joinToken userPlayerId ->
            InternetGameConfiguration
                { players = players
                , mapId = mapId
                , userPlayerId = Player.Id userPlayerId
                , joinToken = JoinToken joinToken
                }
        )
        (Api.Object.InternetGameConfiguration.players playerConfigurationSelectionSet)
        (Graphql.SelectionSet.map Map.Id Api.Object.InternetGameConfiguration.mapId)
        Api.Object.InternetGameConfiguration.joinToken
        Api.Object.InternetGameConfiguration.userPlayerId


playerConfigurationSelectionSet : Graphql.SelectionSet.SelectionSet PlayerConfiguration Api.Object.InternetGamePlayerConfiguration
playerConfigurationSelectionSet =
    Graphql.SelectionSet.map3
        PlayerConfiguration
        (Graphql.SelectionSet.map Player.Id Api.Object.InternetGamePlayerConfiguration.playerId)
        (Api.Object.InternetGamePlayerConfiguration.color Colors.selectionSet)
        Api.Object.InternetGamePlayerConfiguration.name


selectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Union.InternetGame
selectionSet =
    let
        gameSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.Game
        gameSelectionSet =
            Graphql.SelectionSet.map InternetGame Game.selectionSet
    in
    Api.Union.InternetGame.fragments
        { onInternetGameConfiguration = configurationSelectionSet
        , onGame = gameSelectionSet
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
