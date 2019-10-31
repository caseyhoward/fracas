module UserMap exposing
    ( Id(..)
    , UserMap
    , create
    , getAll
    , idToString
    , selectionSet
    )

import Api.InputObject
import Api.Mutation
import Api.Object as ApiObject
import Api.Object.BodyOfWater
import Api.Object.Dimensions
import Api.Object.Map
import Api.Query
import Country
import Dict
import Graphql.Http
import Graphql.SelectionSet exposing (SelectionSet)
import Map
import RemoteData
import Set


type Id
    = Id String


idToString : Id -> String
idToString (Id id) =
    id


type Error
    = Error String


type alias UserMap =
    { id : Id
    , map : Map.Map
    }


getAll : String -> (RemoteData.RemoteData (Graphql.Http.Error (List UserMap)) (List UserMap) -> msg) -> Cmd msg
getAll apiUrl toMsg =
    Api.Query.maps selectionSet
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


get : Id -> Dict.Dict String UserMap -> Result Error UserMap
get (Id id) gameMaps =
    case Dict.get id gameMaps of
        Just gameMap ->
            Ok gameMap

        Nothing ->
            Error "Game map not found" |> Err


selectionSet : SelectionSet UserMap ApiObject.Map
selectionSet =
    let
        bodyOfWaterSelection : SelectionSet ( String, Set.Set String ) ApiObject.BodyOfWater
        bodyOfWaterSelection =
            Graphql.SelectionSet.map2
                (\id neighboringCountries ->
                    ( id, neighboringCountries |> Set.fromList )
                )
                Api.Object.BodyOfWater.id
                Api.Object.BodyOfWater.neighboringCountries

        dimensionsSelection : SelectionSet ( Int, Int ) ApiObject.Dimensions
        dimensionsSelection =
            Graphql.SelectionSet.map2
                (\width height ->
                    ( width, height )
                )
                Api.Object.Dimensions.width
                Api.Object.Dimensions.height

        bodiesOfWaterSelectionToBodiesOfWater : List ( String, Set.Set String ) -> Dict.Dict String (Set.Set String)
        bodiesOfWaterSelectionToBodiesOfWater waterSelectionSet =
            waterSelectionSet |> Dict.fromList
    in
    Graphql.SelectionSet.map5
        (\id name countries bodiesOfWater dimensions ->
            { id = Id id
            , map =
                { name = name
                , countries = countries |> Country.selectionSetsToCountries
                , bodiesOfWater = bodiesOfWater |> bodiesOfWaterSelectionToBodiesOfWater
                , dimensions = dimensions
                }
            }
        )
        Api.Object.Map.id
        Api.Object.Map.name
        (Api.Object.Map.countries Country.selectionSet)
        (Api.Object.Map.bodiesOfWater bodyOfWaterSelection)
        (Api.Object.Map.dimensions dimensionsSelection)


create : String -> Map.Map -> (RemoteData.RemoteData (Graphql.Http.Error UserMap) UserMap -> msg) -> Cmd msg
create apiUrl newUserMap toMsg =
    let
        countryInputs : List Api.InputObject.CountryInput
        countryInputs =
            newUserMap.countries
                |> Country.countryInputs

        bodiesOfWater : List Api.InputObject.BodyOfWaterInput
        bodiesOfWater =
            newUserMap.bodiesOfWater
                |> Dict.map
                    (\waterId countries ->
                        { id = waterId, neighboringCountries = countries |> Set.toList }
                    )
                |> Dict.values

        dimensionsInput : Api.InputObject.DimensionsInput
        dimensionsInput =
            case newUserMap.dimensions of
                ( width, height ) ->
                    { width = width, height = height }

        requiredFields : Api.InputObject.MapInputRequiredFields
        requiredFields =
            { name = newUserMap.name
            , countries = countryInputs
            , bodiesOfWater = bodiesOfWater
            , dimensions = dimensionsInput
            }

        input : Api.InputObject.MapInput
        input =
            requiredFields |> Api.InputObject.buildMapInput
    in
    Api.Mutation.createMap { map = input } selectionSet
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)
