module Route exposing
    ( Route(..)
    , fromUrl
    , href
    , pushUrl
    , replaceUrl
    )

import ActiveGame
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Player
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- EXPOSED


type Route
    = ConfiguringGame
    | NewMap
    | Game ActiveGame.Id ActiveGame.PlayerId


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
        , Parser.map Game (s "games" </> ActiveGame.urlParser </> ActiveGame.playerUrlParser)
        , Parser.map NewMap (s "maps" </> s "new")
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                ConfiguringGame ->
                    [ "games", "new" ]

                Game gameId playerId ->
                    [ "games", ActiveGame.idToString gameId, ActiveGame.playerIdToString playerId ]

                NewMap ->
                    [ "maps", "new" ]
    in
    "/" ++ String.join "/" pieces
