module TroopCount exposing
    ( TroopCount
    , acrossWater
    , addTroopCounts
    , canAttack
    , hasTroops
    , noTroops
    , numberOfTroopsToMove
    , numberOfTroopsToPlace
    , pluralize
    , random
    , subtractTroopCounts
    , toString
    )

import Random


type TroopCount
    = TroopCount Int


acrossWater : TroopCount -> TroopCount
acrossWater (TroopCount troopCount) =
    toFloat troopCount * 0.25 |> floor |> TroopCount


addTroopCounts : TroopCount -> TroopCount -> TroopCount
addTroopCounts (TroopCount troopCount1) (TroopCount troopCount2) =
    TroopCount (troopCount1 + troopCount2)


canAttack : TroopCount -> TroopCount -> Bool
canAttack (TroopCount attackTroopCount) (TroopCount defenseTroopCount) =
    attackTroopCount > defenseTroopCount


hasTroops : TroopCount -> Bool
hasTroops (TroopCount troopCount) =
    troopCount > 0


noTroops : TroopCount
noTroops =
    TroopCount 0


numberOfTroopsToPlace : Int -> Int -> TroopCount
numberOfTroopsToPlace numberOfCountries troopsPerCountryPerTurn =
    numberOfCountries * troopsPerCountryPerTurn |> TroopCount


numberOfTroopsToMove : TroopCount -> Int -> TroopCount
numberOfTroopsToMove (TroopCount maximumNumberOfTroops) attemptedNumberOfTroops =
    TroopCount
        (if attemptedNumberOfTroops > maximumNumberOfTroops then
            maximumNumberOfTroops

         else
            attemptedNumberOfTroops
        )


pluralize : TroopCount -> String
pluralize (TroopCount troopCount) =
    if troopCount > 1 then
        String.fromInt troopCount ++ " troops"

    else
        String.fromInt troopCount ++ " troop"


random : Int -> Random.Generator TroopCount
random maximumNeutralCountryTroops =
    Random.int 1 maximumNeutralCountryTroops |> Random.map TroopCount


subtractTroopCounts : TroopCount -> TroopCount -> TroopCount
subtractTroopCounts (TroopCount ammountToSubtract) (TroopCount subtractFrom) =
    TroopCount (subtractFrom - ammountToSubtract)


toString : TroopCount -> String
toString (TroopCount troopCount) =
    String.fromInt troopCount
