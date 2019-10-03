module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , pushUrl
    , replaceUrl
    )

import ActiveGame
import Browser.Navigation as Nav
import Game
import Html exposing (Attribute)
import Html.Attributes as Attr
import Map
import Player
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- EXPOSED


type Route
    = ConfiguringGame
    | ActiveGame ActiveGame.Id
    | EditMap Map.Id
    | NewMap
    | Game Game.Id Player.Id


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
        , Parser.map ActiveGame (s "active-games" </> ActiveGame.urlParser)

        -- , Parser.map Game (s "games" </> Game.urlParser)
        , Parser.map Game (s "games" </> Game.urlParser </> Player.urlParser)
        , Parser.map NewMap (s "maps" </> s "new")
        , Parser.map EditMap (s "maps" </> Map.urlParser)
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                ConfiguringGame ->
                    [ "games", "new" ]

                ActiveGame activeGameId ->
                    [ "active-games/", ActiveGame.idToString activeGameId ]

                EditMap mapId ->
                    [ "maps", Map.idToString mapId ]

                Game gameId playerId ->
                    [ "games", Game.idToString gameId, Player.idToString playerId ]

                NewMap ->
                    [ "maps", "new" ]
    in
    "/" ++ String.join "/" pieces
