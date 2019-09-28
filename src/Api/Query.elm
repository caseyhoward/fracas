-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Query exposing (..)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)


type alias MapOptionalArguments =
    { id : OptionalArgument String }


{-|

  - id -

-}
map : (MapOptionalArguments -> MapOptionalArguments) -> SelectionSet decodesTo Api.Object.Map -> SelectionSet (Maybe decodesTo) RootQuery
map fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { id = Absent }

        optionalArgs =
            [ Argument.optional "id" filledInOptionals.id Encode.string ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "map" optionalArgs object_ (identity >> Decode.nullable)


{-| -}
maps : SelectionSet decodesTo Api.Object.Map -> SelectionSet (List decodesTo) RootQuery
maps object_ =
    Object.selectionForCompositeField "maps" [] object_ (identity >> Decode.list)
