module InternetGame exposing (Configuration, GameOrConfiguration(..), InternetGameTokens, PlayerToken, create, get, playerTokenToString, playerTokenUrlParser)

-- import Api.Object

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
import RemoteData
import Url.Parser


type PlayerId
    = PlayerId Int


type alias PlayerConfiguration =
    { id : PlayerId
    , color : Colors.Color
    , name : String
    }


type alias Configuration =
    { players : List PlayerConfiguration, mapId : Map.Id, userPlayerId : PlayerId }


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


create : (RemoteData.RemoteData (Graphql.Http.Error PlayerToken) PlayerToken -> msg) -> Cmd msg
create toMsg =
    (Api.Mutation.createInternetGame
        |> Graphql.SelectionSet.map PlayerToken
    )
        |> Graphql.Http.mutationRequest "http://192.168.1.7:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


get : PlayerToken -> (RemoteData.RemoteData (Graphql.Http.Error GameOrConfiguration) GameOrConfiguration -> msg) -> Cmd msg
get playerToken toMsg =
    let
        playerConfigurationSelectionSet : Graphql.SelectionSet.SelectionSet PlayerConfiguration Api.Object.InternetGamePlayerConfiguration
        playerConfigurationSelectionSet =
            Graphql.SelectionSet.map3
                PlayerConfiguration
                (Graphql.SelectionSet.map PlayerId Api.Object.InternetGamePlayerConfiguration.playerId)
                (Api.Object.InternetGamePlayerConfiguration.color Colors.selectionSet)
                Api.Object.InternetGamePlayerConfiguration.name

        configurationSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.InternetGameConfiguration
        configurationSelectionSet =
            Graphql.SelectionSet.map3
                (\players mapId userPlayerId ->
                    InternetGameConfiguration
                        { players = players
                        , mapId = mapId
                        , userPlayerId = PlayerId userPlayerId
                        }
                )
                (Api.Object.InternetGameConfiguration.players playerConfigurationSelectionSet)
                (Graphql.SelectionSet.map Map.Id Api.Object.InternetGameConfiguration.mapId)
                Api.Object.InternetGameConfiguration.userPlayerId

        gameSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.Game
        gameSelectionSet =
            Graphql.SelectionSet.map InternetGame Game.selectionSet

        selectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Union.InternetGame
        selectionSet =
            Api.Union.InternetGame.fragments
                { onInternetGameConfiguration = configurationSelectionSet
                , onGame = gameSelectionSet
                }
    in
    Api.Query.internetGame
        { playerToken = playerToken |> playerTokenToString }
        selectionSet
        |> Graphql.Http.queryRequest "http://192.168.1.7:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


playerTokenToString : PlayerToken -> String
playerTokenToString (PlayerToken token) =
    token


playerTokenUrlParser : Url.Parser.Parser (PlayerToken -> a) a
playerTokenUrlParser =
    Url.Parser.custom "PLAYERTOKEN" (\playerId -> playerId |> PlayerToken |> Just)
