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


type alias MapRequiredArguments =
    { id : String }


{-|

  - id -

-}
map : MapRequiredArguments -> SelectionSet decodesTo Api.Object.Map -> SelectionSet decodesTo RootQuery
map requiredArgs object_ =
    Object.selectionForCompositeField "map" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


type alias GameRequiredArguments =
    { id : String }


{-|

  - id -

-}
game : GameRequiredArguments -> SelectionSet decodesTo Api.Object.Game -> SelectionSet decodesTo RootQuery
game requiredArgs object_ =
    Object.selectionForCompositeField "game" [ Argument.required "id" requiredArgs.id Encode.string ] object_ identity


{-| -}
maps : SelectionSet decodesTo Api.Object.Map -> SelectionSet (List decodesTo) RootQuery
maps object_ =
    Object.selectionForCompositeField "maps" [] object_ (identity >> Decode.list)
