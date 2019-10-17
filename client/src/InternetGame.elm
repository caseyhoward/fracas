module InternetGame exposing
    ( Configuration
    , GameOrConfiguration(..)
    , InternetGameTokens
    , JoinToken
    , PlayerToken
    , create
    , gameOrConfigurationSelectionSet
    , gameSelectionSet
    , get
    , joinGame
    , joinTokenToString
    , joinTokenUrlParser
    , playerTokenToString
    , playerTokenUrlParser
    , save
    , selectionSet
    , start
    , updateColor
    , updateMap
    , updatePlayerName
    )

import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Game
import Api.Object.InternetGame
import Api.Object.InternetGameConfiguration
import Api.Object.InternetGamePlayerConfiguration
import Api.Query
import Api.Union
import Api.Union.InternetGameOrConfiguration
import Colors
import Dict
import Game
import Graphql.Http
import Graphql.SelectionSet
import Map
import Player
import PlayerTurn
import RemoteData
import TroopCount
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
    , isCurrentUserHost : Bool
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
    = InternetGame Game.GameWithCurrentUser
    | InternetGameConfiguration Configuration


create : String -> (RemoteData.RemoteData (Graphql.Http.Error PlayerToken) PlayerToken -> msg) -> Cmd msg
create apiUrl toMsg =
    (Api.Mutation.createInternetGame
        |> Graphql.SelectionSet.map PlayerToken
    )
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


get : String -> PlayerToken -> (RemoteData.RemoteData (Graphql.Http.Error Game.GameWithCurrentUser) Game.GameWithCurrentUser -> msg) -> Cmd msg
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


start : String -> PlayerToken -> (RemoteData.RemoteData (Graphql.Http.Error Bool) Bool -> msg) -> Cmd msg
start apiUrl (PlayerToken playerToken) toMsg =
    Api.Mutation.startInternetGame { playerToken = playerToken }
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


save : String -> PlayerToken -> Game.Game -> (RemoteData.RemoteData (Graphql.Http.Error Bool) Bool -> msg) -> Cmd msg
save apiUrl (PlayerToken playerToken) game toMsg =
    let
        gameInput : Api.InputObject.GameInput
        gameInput =
            Api.InputObject.buildGameInput
                { id = game.id |> Game.idToString
                , mapId = game.map.id |> Map.idToString
                , players = game.players |> Player.input
                , neutralCountryTroops = game.neutralCountryTroops |> TroopCount.troopCountsInput
                , playerTurn = game.currentPlayerTurn |> PlayerTurn.input
                }
    in
    Api.Mutation.saveInternetGame { playerToken = playerToken, game = gameInput }
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


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


updateMap : String -> PlayerToken -> Map.Id -> (RemoteData.RemoteData (Graphql.Http.Error Bool) Bool -> msg) -> Cmd msg
updateMap apiUrl playerToken mapId toMsg =
    Api.Mutation.updateMapForInternetGame
        { playerToken = playerToken |> playerTokenToString, mapId = mapId |> Map.idToString }
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


selectionSet : Graphql.SelectionSet.SelectionSet Game.GameWithCurrentUser Api.Object.InternetGame
selectionSet =
    Graphql.SelectionSet.map2 (\game currentUserPlayerId -> { game = game, currentUserPlayerId = Player.Id currentUserPlayerId })
        (Api.Object.InternetGame.game gameSelectionSet)
        Api.Object.InternetGame.currentUserPlayerId


gameOrConfigurationSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Union.InternetGameOrConfiguration
gameOrConfigurationSelectionSet =
    Api.Union.InternetGameOrConfiguration.fragments
        { onInternetGameConfiguration = configurationSelectionSet
        , onInternetGame = internetGameSelectionSet
        }


internetGameSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.InternetGame
internetGameSelectionSet =
    Graphql.SelectionSet.map2 (\game currentUserPlayerId -> InternetGame { game = game, currentUserPlayerId = Player.Id currentUserPlayerId })
        (Api.Object.InternetGame.game gameSelectionSet)
        Api.Object.InternetGame.currentUserPlayerId


configurationSelectionSet1 : Graphql.SelectionSet.SelectionSet Configuration Api.Object.InternetGameConfiguration
configurationSelectionSet1 =
    Graphql.SelectionSet.map5
        (\players mapId joinToken currentUserPlayerId isCurrentUserHost ->
            { players = players
            , mapId = mapId
            , currentUserPlayerId = Player.Id currentUserPlayerId
            , joinToken = JoinToken joinToken
            , isCurrentUserHost = isCurrentUserHost
            }
        )
        (Api.Object.InternetGameConfiguration.players playerConfigurationSelectionSet)
        (Graphql.SelectionSet.map Map.Id Api.Object.InternetGameConfiguration.mapId)
        Api.Object.InternetGameConfiguration.joinToken
        Api.Object.InternetGameConfiguration.currentUserPlayerId
        Api.Object.InternetGameConfiguration.isCurrentUserHost


configurationSelectionSet : Graphql.SelectionSet.SelectionSet GameOrConfiguration Api.Object.InternetGameConfiguration
configurationSelectionSet =
    Graphql.SelectionSet.map InternetGameConfiguration configurationSelectionSet1


gameSelectionSet : Graphql.SelectionSet.SelectionSet Game.Game Api.Object.Game
gameSelectionSet =
    Graphql.SelectionSet.map5
        (\id2 map currentPlayerTurn players neutralCountryTroops ->
            let
                activeGame : Game.Game
                activeGame =
                    { id = Game.Id id2
                    , currentPlayerTurn = currentPlayerTurn
                    , map = map
                    , players = players |> Player.playerSelectionSetsToPlayers
                    , neutralCountryTroops = neutralCountryTroops |> Dict.fromList
                    }
            in
            activeGame
        )
        Api.Object.Game.id
        (Api.Object.Game.map Map.mapSelection)
        (Api.Object.Game.playerTurn PlayerTurn.selectionSet)
        (Api.Object.Game.players Player.playerSelection)
        (Api.Object.Game.neutralCountryTroops TroopCount.troopCountsSelection)


playerTokenToString : PlayerToken -> String
playerTokenToString (PlayerToken token) =
    token


playerTokenUrlParser : Url.Parser.Parser (PlayerToken -> a) a
playerTokenUrlParser =
    Url.Parser.custom "PLAYERTOKEN" (\playerId -> playerId |> PlayerToken |> Just)


joinTokenUrlParser : Url.Parser.Parser (JoinToken -> a) a
joinTokenUrlParser =
    Url.Parser.custom "JOINTOKEN" (\joinToken -> joinToken |> JoinToken |> Just)


playerConfigurationSelectionSet : Graphql.SelectionSet.SelectionSet PlayerConfiguration Api.Object.InternetGamePlayerConfiguration
playerConfigurationSelectionSet =
    Graphql.SelectionSet.map3
        PlayerConfiguration
        (Graphql.SelectionSet.map Player.Id Api.Object.InternetGamePlayerConfiguration.playerId)
        (Api.Object.InternetGamePlayerConfiguration.color Colors.selectionSet)
        Api.Object.InternetGamePlayerConfiguration.name
