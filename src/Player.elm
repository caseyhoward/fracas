module Player exposing
    ( CapitolStatus(..)
    , Id(..)
    , Player
    , PlayerSelectionSet
    , Players
    , addPort
    , createDefaultPlayers
    ,  idToString
       -- , urlParser

    , input
    )

import Api.InputObject
import Colors
import Country
import Dict
import Set
import TroopCount
import Url.Parser


type alias Player =
    { name : String
    , countryTroopCounts : Dict.Dict String TroopCount.TroopCount
    , capitolStatus : CapitolStatus
    , color : Colors.Color
    , ports : Set.Set String
    }


type alias Players =
    Dict.Dict Int Player


type Id
    = Id Int


type CapitolStatus
    = NoCapitol
    | Capitol Country.Id


idToString : Id -> String
idToString (Id id) =
    id |> String.fromInt



-- urlParser : Url.Parser.Parser (Id -> a) a
-- urlParser =
--     Url.Parser.custom "PLAYERID" (\playerId -> Just (Id playerId))
---- GRAPHQL ----


type alias PlayerSelectionSet =
    { id : Id
    , name : String
    , countryTroopCounts : Dict.Dict String TroopCount.TroopCount
    , capitolStatus : CapitolStatus
    , color : Colors.Color
    , ports : Set.Set String
    }


createDefaultPlayers : Int -> Players
createDefaultPlayers numberOfPlayers =
    List.range 1 numberOfPlayers
        |> List.map
            (\playerId ->
                let
                    player : Player
                    player =
                        { countryTroopCounts = Dict.empty
                        , name = "Player " ++ String.fromInt playerId
                        , capitolStatus = NoCapitol
                        , color = getDefaultColor (Id playerId)
                        , ports = Set.empty
                        }
                in
                ( playerId, player )
            )
        |> Dict.fromList


input : Players -> List Api.InputObject.PlayerInput
input players =
    players
        |> Dict.map
            (\playerId player ->
                let
                    fields : Api.InputObject.PlayerInputRequiredFields
                    fields =
                        { id = playerId |> String.fromInt
                        , countryTroopCounts = player.countryTroopCounts |> TroopCount.troopCountsInput
                        , name = player.name
                        , color = player.color |> Colors.input
                        , ports = []
                        }
                in
                Api.InputObject.buildPlayerInput
                    fields
                    identity
            )
        |> Dict.values


addPort : Country.Id -> Player -> Player
addPort (Country.Id countryId) player =
    { player | ports = player.ports |> Set.insert countryId }


playerIdToString : Id -> String
playerIdToString (Id playerId) =
    String.fromInt playerId


defaultPlayerColors : Dict.Dict Int Colors.Color
defaultPlayerColors =
    Dict.fromList
        [ ( 1, Colors.darkGreen )
        , ( 3, Colors.lightGreen )
        , ( 2, Colors.lightYellow )
        , ( 5, Colors.orange )
        , ( 4, Colors.brown )
        , ( 6, Colors.lightPurple )
        ]


getDefaultColor : Id -> Colors.Color
getDefaultColor (Id playerId) =
    case Dict.get playerId defaultPlayerColors of
        Just color ->
            color

        Nothing ->
            Colors.black
