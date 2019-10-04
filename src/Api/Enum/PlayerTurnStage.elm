-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.PlayerTurnStage exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-|

  - CapitolPlacement -
  - TroopPlacement -
  - AttackAnnexOrPort -
  - TroopMovement -
  - GameOver -

-}
type PlayerTurnStage
    = CapitolPlacement
    | TroopPlacement
    | AttackAnnexOrPort
    | TroopMovement
    | GameOver


list : List PlayerTurnStage
list =
    [ CapitolPlacement, TroopPlacement, AttackAnnexOrPort, TroopMovement, GameOver ]


decoder : Decoder PlayerTurnStage
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "CapitolPlacement" ->
                        Decode.succeed CapitolPlacement

                    "TroopPlacement" ->
                        Decode.succeed TroopPlacement

                    "AttackAnnexOrPort" ->
                        Decode.succeed AttackAnnexOrPort

                    "TroopMovement" ->
                        Decode.succeed TroopMovement

                    "GameOver" ->
                        Decode.succeed GameOver

                    _ ->
                        Decode.fail ("Invalid PlayerTurnStage type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : PlayerTurnStage -> String
toString enum =
    case enum of
        CapitolPlacement ->
            "CapitolPlacement"

        TroopPlacement ->
            "TroopPlacement"

        AttackAnnexOrPort ->
            "AttackAnnexOrPort"

        TroopMovement ->
            "TroopMovement"

        GameOver ->
            "GameOver"
