-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.PlayerTurn exposing (..)

import Api.Enum.PlayerTurnStage
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


playerId : SelectionSet String Api.Object.PlayerTurn
playerId =
    Object.selectionForField "String" "playerId" [] Decode.string


playerTurnStage : SelectionSet Api.Enum.PlayerTurnStage.PlayerTurnStage Api.Object.PlayerTurn
playerTurnStage =
    Object.selectionForField "Enum.PlayerTurnStage.PlayerTurnStage" "playerTurnStage" [] Api.Enum.PlayerTurnStage.decoder


fromCountryId : SelectionSet (Maybe String) Api.Object.PlayerTurn
fromCountryId =
    Object.selectionForField "(Maybe String)" "fromCountryId" [] (Decode.string |> Decode.nullable)


troopCount : SelectionSet (Maybe String) Api.Object.PlayerTurn
troopCount =
    Object.selectionForField "(Maybe String)" "troopCount" [] (Decode.string |> Decode.nullable)
