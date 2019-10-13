module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , pushUrl
    , replaceUrl
    )

import Browser.Navigation as Nav
import Game
import Html exposing (Attribute)
import Html.Attributes as Attr
import InternetGame
import Player
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- EXPOSED


type Route
    = ConfiguringGame
    | NewMap
    | Game Game.Id Player.Id
    | InternetGame InternetGame.PlayerToken
    | JoinInternetGame String


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    url |> Parser.parse parser



-- INTERNAL


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map ConfiguringGame Parser.top
        , Parser.map ConfiguringGame (s "games" </> s "new")
        , Parser.map Game (s "games" </> Game.urlParser </> s "players" </> Player.urlParser)
        , Parser.map InternetGame (s "games" </> s "internet" </> InternetGame.playerTokenUrlParser)
        , Parser.map JoinInternetGame (s "games" </> s "internet" </> s "join" </> Parser.string)
        , Parser.map NewMap (s "maps" </> s "new")
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                ConfiguringGame ->
                    [ "games", "new" ]

                JoinInternetGame joinGameKey ->
                    [ "games", "internet", "join", joinGameKey ]

                InternetGame playerKey ->
                    [ "games", "internet", playerKey |> InternetGame.playerTokenToString ]

                Game gameId playerId ->
                    [ "games", Game.idToString gameId, "players", Player.idToString playerId ]

                NewMap ->
                    [ "maps", "new" ]
    in
    "/" ++ String.join "/" pieces