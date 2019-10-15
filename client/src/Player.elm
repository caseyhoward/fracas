module Player exposing
    ( CapitolStatus(..)
    , Id(..)
    , NewPlayer
    , Player
    , PlayerSelectionSet
    , Players
    , addPort
    , availablePlayerColors
    , defaultNewPlayers
    , getPlayer
    , getPlayerName
    , idToString, hostPlayerId
    , input
    , newPlayersInput
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


type alias NewPlayer =
    { name : String
    , color : Colors.Color
    }


defaultNewPlayers : Dict.Dict Int NewPlayer
defaultNewPlayers =
    [ ( 0, { name = "Kevin", color = Colors.darkGreen } )
    , ( 1, { name = "Jim", color = Colors.lightGreen } )
    , ( 2, { name = "Lyle", color = Colors.lightYellow } )
    , ( 3, { name = "Nat", color = Colors.orange } )
    , ( 4, { name = "Denny", color = Colors.brown } )
    , ( 5, { name = "Jason", color = Colors.lightPurple } )
    ]
        |> Dict.fromList


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


hostPlayerId : Dict.Dict Int Player -> Id
hostPlayerId players =
    case players |> Dict.keys |> List.head of
        Just host ->
            Id host

        Nothing ->
            Id -1


addPort : Country.Id -> Player -> Player
addPort (Country.Id countryId) player =
    { player | ports = player.ports |> Set.insert countryId }


availablePlayerColors : Dict.Dict Int NewPlayer -> List Colors.Color
availablePlayerColors players =
    let
        takenColors =
            players |> Dict.values |> List.map .color
    in
    [ Colors.lightBrown
    , Colors.brown
    , Colors.darkBrown
    , Colors.lightGreen
    , Colors.green
    , Colors.darkGreen
    , Colors.lightBlue
    , Colors.lightGray
    , Colors.gray
    , Colors.darkGray
    , Colors.charcoal
    , Colors.darkCharcoal
    , Colors.lightOrange
    , Colors.orange
    , Colors.darkOrange
    , Colors.lightPurple
    , Colors.purple
    , Colors.darkPurple
    , Colors.lightRed
    , Colors.red
    , Colors.darkRed
    , Colors.lightYellow
    , Colors.darkYellow
    , Colors.yellow
    , Colors.white
    ]
        |> List.filter (\color -> not (List.member color takenColors))


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


newPlayerToPlayer : NewPlayer -> Player
newPlayerToPlayer newPlayer =
    -- Move to server
    { countryTroopCounts = Dict.empty
    , name = newPlayer.name
    , capitolStatus = NoCapitol
    , color = newPlayer.color
    , ports = Set.empty
    }


newPlayersInput : List NewPlayer -> List Api.InputObject.PlayerInput
newPlayersInput newPlayers =
    newPlayers |> List.map newPlayerToPlayer |> List.indexedMap (\index player -> ( index, player )) |> Dict.fromList |> input


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
