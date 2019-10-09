module Player exposing
    ( CapitolStatus(..)
    , Id(..)
    , Player
    , PlayerSelectionSet
    , Players
    , addPort
    , createDefaultPlayers
    , getPlayer
    , getPlayerName
    , idToString
    , input
    , numberOfTroopsToPlace
    , playerSelection
    , playerSelectionSetsToPlayers
    , urlParser
    )

import Api.InputObject
import Api.Object
import Api.Object.Player
import Colors
import Country
import Dict
import Graphql.OptionalArgument
import Graphql.SelectionSet exposing (SelectionSet)
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


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "PLAYERID" (\playerId -> playerId |> String.toInt |> Maybe.map Id)


addPort : Country.Id -> Player -> Player
addPort (Country.Id countryId) player =
    { player | ports = player.ports |> Set.insert countryId }


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


getPlayer : Id -> Players -> Maybe Player
getPlayer (Id playerId) players =
    Dict.get playerId players


getPlayerName : Id -> Players -> Maybe String
getPlayerName playerId players =
    getPlayer playerId players
        |> Maybe.map .name


playerSelection : SelectionSet PlayerSelectionSet Api.Object.Player
playerSelection =
    Graphql.SelectionSet.map6
        (\playerId name countryTroopCounts maybeCapitol color ports ->
            let
                capitol =
                    case maybeCapitol of
                        Just countryId ->
                            Capitol (Country.Id countryId)

                        Nothing ->
                            NoCapitol
            in
            { id = playerId |> String.toInt |> Maybe.withDefault 0 |> Id
            , name = name
            , countryTroopCounts = countryTroopCounts |> Dict.fromList
            , capitolStatus = capitol
            , color = color
            , ports = ports |> Set.fromList
            }
        )
        Api.Object.Player.id
        Api.Object.Player.name
        (Api.Object.Player.countryTroopCounts TroopCount.troopCountsSelection)
        Api.Object.Player.capitol
        (Api.Object.Player.color Colors.selectionSet)
        Api.Object.Player.ports


playerSelectionSetsToPlayers : List PlayerSelectionSet -> Players
playerSelectionSetsToPlayers playerSelectionSets =
    playerSelectionSets
        |> List.map
            (\playerSelectionSet ->
                case playerSelectionSet.id of
                    Id playerId ->
                        ( playerId
                        , { name = playerSelectionSet.name
                          , countryTroopCounts = playerSelectionSet.countryTroopCounts
                          , capitolStatus = playerSelectionSet.capitolStatus
                          , color = playerSelectionSet.color
                          , ports = playerSelectionSet.ports
                          }
                        )
            )
        |> Dict.fromList


numberOfTroopsToPlace : Id -> Players -> TroopCount.TroopCount
numberOfTroopsToPlace playerId players =
    case getPlayer playerId players of
        Just player ->
            TroopCount.numberOfTroopsToPlace (Dict.size player.countryTroopCounts) troopsPerCountryPerTurn

        Nothing ->
            TroopCount.noTroops


troopsPerCountryPerTurn : Int
troopsPerCountryPerTurn =
    1



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
                        , ports = player.ports |> Set.toList
                        }
                in
                Api.InputObject.buildPlayerInput
                    fields
                    (\x ->
                        { x
                            | capitol =
                                (case player.capitolStatus of
                                    Capitol capitolId ->
                                        Just (Country.idToString capitolId)

                                    NoCapitol ->
                                        Nothing
                                )
                                    |> Graphql.OptionalArgument.fromMaybe
                        }
                    )
            )
        |> Dict.values
