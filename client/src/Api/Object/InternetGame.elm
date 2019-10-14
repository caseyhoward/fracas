-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.InternetGame exposing (..)

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
import Json.Decode as Decode


{-| -}
game : SelectionSet decodesTo Api.Object.Game -> SelectionSet decodesTo Api.Object.InternetGame
game object_ =
    Object.selectionForCompositeField "game" [] object_ identity


{-| -}
currentUserPlayerId : SelectionSet Int Api.Object.InternetGame
currentUserPlayerId =
    Object.selectionForField "Int" "currentUserPlayerId" [] Decode.int
