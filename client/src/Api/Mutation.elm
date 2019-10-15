-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Mutation exposing (..)

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


type alias CreateGameRequiredArguments =
    { newGame : Api.InputObject.NewGameInput }


{-|

  - newGame -

-}
createGame : CreateGameRequiredArguments -> SelectionSet decodesTo Api.Object.Game -> SelectionSet decodesTo RootMutation
createGame requiredArgs object_ =
    Object.selectionForCompositeField "createGame" [ Argument.required "newGame" requiredArgs.newGame Api.InputObject.encodeNewGameInput ] object_ identity


{-| -}
createInternetGame : SelectionSet String RootMutation
createInternetGame =
    Object.selectionForField "String" "createInternetGame" [] Decode.string


type alias CreateMapRequiredArguments =
    { map : Api.InputObject.MapInput }


{-|

  - map -

-}
createMap : CreateMapRequiredArguments -> SelectionSet decodesTo Api.Object.Map -> SelectionSet decodesTo RootMutation
createMap requiredArgs object_ =
    Object.selectionForCompositeField "createMap" [ Argument.required "map" requiredArgs.map Api.InputObject.encodeMapInput ] object_ identity


type alias JoinInternetGameRequiredArguments =
    { joinGameToken : String }


{-|

  - joinGameToken -

-}
joinInternetGame : JoinInternetGameRequiredArguments -> SelectionSet String RootMutation
joinInternetGame requiredArgs =
    Object.selectionForField "String" "joinInternetGame" [ Argument.required "joinGameToken" requiredArgs.joinGameToken Encode.string ] Decode.string


type alias RemovePlayerRequiredArguments =
    { playerToken : String
    , playerId : Int
    }


{-|

  - playerToken -
  - playerId -

-}
removePlayer : RemovePlayerRequiredArguments -> SelectionSet decodesTo Api.Object.Game -> SelectionSet decodesTo RootMutation
removePlayer requiredArgs object_ =
    Object.selectionForCompositeField "removePlayer" [ Argument.required "playerToken" requiredArgs.playerToken Encode.string, Argument.required "playerId" requiredArgs.playerId Encode.int ] object_ identity


type alias SaveGameRequiredArguments =
    { game : Api.InputObject.GameInput }


{-|

  - game -

-}
saveGame : SaveGameRequiredArguments -> SelectionSet decodesTo Api.Object.Game -> SelectionSet decodesTo RootMutation
saveGame requiredArgs object_ =
    Object.selectionForCompositeField "saveGame" [ Argument.required "game" requiredArgs.game Api.InputObject.encodeGameInput ] object_ identity


type alias StartInternetGameRequiredArguments =
    { playerToken : String }


{-|

  - playerToken -

-}
startInternetGame : StartInternetGameRequiredArguments -> SelectionSet decodesTo Api.Object.Game -> SelectionSet decodesTo RootMutation
startInternetGame requiredArgs object_ =
    Object.selectionForCompositeField "startInternetGame" [ Argument.required "playerToken" requiredArgs.playerToken Encode.string ] object_ identity


type alias UpdateMapForInternetGameRequiredArguments =
    { playerToken : String
    , mapId : String
    }


{-|

  - playerToken -
  - mapId -

-}
updateMapForInternetGame : UpdateMapForInternetGameRequiredArguments -> SelectionSet decodesTo Api.Object.InternetGameConfiguration -> SelectionSet decodesTo RootMutation
updateMapForInternetGame requiredArgs object_ =
    Object.selectionForCompositeField "updateMapForInternetGame" [ Argument.required "playerToken" requiredArgs.playerToken Encode.string, Argument.required "mapId" requiredArgs.mapId Encode.string ] object_ identity


type alias UpdatePlayerNameForInternetGameRequiredArguments =
    { name : String
    , playerToken : String
    }


{-|

  - name -
  - playerToken -

-}
updatePlayerNameForInternetGame : UpdatePlayerNameForInternetGameRequiredArguments -> SelectionSet Bool RootMutation
updatePlayerNameForInternetGame requiredArgs =
    Object.selectionForField "Bool" "updatePlayerNameForInternetGame" [ Argument.required "name" requiredArgs.name Encode.string, Argument.required "playerToken" requiredArgs.playerToken Encode.string ] Decode.bool


type alias UpdatePlayerColorForInternetGameRequiredArguments =
    { color : Api.InputObject.ColorInput
    , playerToken : String
    }


{-|

  - color -
  - playerToken -

-}
updatePlayerColorForInternetGame : UpdatePlayerColorForInternetGameRequiredArguments -> SelectionSet Bool RootMutation
updatePlayerColorForInternetGame requiredArgs =
    Object.selectionForField "Bool" "updatePlayerColorForInternetGame" [ Argument.required "color" requiredArgs.color Api.InputObject.encodeColorInput, Argument.required "playerToken" requiredArgs.playerToken Encode.string ] Decode.bool
