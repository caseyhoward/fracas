-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Map exposing (..)

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
id : SelectionSet String Api.Object.Map
id =
    Object.selectionForField "String" "id" [] Decode.string


{-| -}
name : SelectionSet String Api.Object.Map
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| -}
countries : SelectionSet decodesTo Api.Object.Country -> SelectionSet (List decodesTo) Api.Object.Map
countries object_ =
    Object.selectionForCompositeField "countries" [] object_ (identity >> Decode.list)


{-| -}
bodiesOfWater : SelectionSet decodesTo Api.Object.BodyOfWater -> SelectionSet (List decodesTo) Api.Object.Map
bodiesOfWater object_ =
    Object.selectionForCompositeField "bodiesOfWater" [] object_ (identity >> Decode.list)


{-| -}
dimensions : SelectionSet decodesTo Api.Object.Dimensions -> SelectionSet decodesTo Api.Object.Map
dimensions object_ =
    Object.selectionForCompositeField "dimensions" [] object_ identity
