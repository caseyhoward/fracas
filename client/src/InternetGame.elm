module InternetGame exposing (InternetGameTokens, NewInternetGame, PlayerToken, create, playerTokenToString, playerTokenUrlParser)

-- import Api.Object

import Api.Mutation
import Graphql.Http
import Graphql.SelectionSet
import RemoteData
import Url.Parser


type alias NewInternetGame =
    {}


type JoinToken
    = JoinToken String


type PlayerToken
    = PlayerToken String


type alias InternetGameTokens =
    { joinToken : JoinToken
    , playerToken : PlayerToken
    }


create : (RemoteData.RemoteData (Graphql.Http.Error PlayerToken) PlayerToken -> msg) -> Cmd msg
create toMsg =
    (Api.Mutation.createInternetGame
        |> Graphql.SelectionSet.map PlayerToken
    )
        |> Graphql.Http.mutationRequest "http://192.168.1.7:4000"
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


playerTokenToString : PlayerToken -> String
playerTokenToString (PlayerToken token) =
    token


playerTokenUrlParser : Url.Parser.Parser (PlayerToken -> a) a
playerTokenUrlParser =
    Url.Parser.custom "PLAYERTOKEN" (\playerId -> playerId |> PlayerToken |> Just)
