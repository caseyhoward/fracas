module Route exposing (Route(..), fromUrl, href, replaceUrl)

import ActiveGame
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Map
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- EXPOSED


type Route
    = ConfiguringGame
    | ActiveGame ActiveGame.Id
    | EditMap Map.Id
    | NewMap


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    url |> Parser.parse parser



-- INTERNAL


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map ConfiguringGame Parser.top
        , Parser.map ActiveGame (s "games" </> ActiveGame.urlParser)
        , Parser.map NewMap (s "maps" </> s "new")
        , Parser.map EditMap (s "maps" </> Map.urlParser)
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                ConfiguringGame ->
                    []

                ActiveGame activeGameId ->
                    [ "games", ActiveGame.idToString activeGameId ]

                EditMap mapId ->
                    [ "maps", Map.idToString mapId ]

                NewMap ->
                    [ "maps", "new" ]
    in
    "/" ++ String.join "/" pieces
