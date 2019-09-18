module TroopCount exposing
    ( TroopCount(..)
    , acrossWater
    , addTroopCounts
    , hasTroops
    , noTroops
    , subtractTroopCounts
    , toString
    )


type TroopCount
    = TroopCount Int


acrossWater : TroopCount -> TroopCount
acrossWater (TroopCount troopCount) =
    toFloat troopCount * 0.25 |> floor |> TroopCount


addTroopCounts : TroopCount -> TroopCount -> TroopCount
addTroopCounts (TroopCount troopCount1) (TroopCount troopCount2) =
    TroopCount (troopCount1 + troopCount2)


hasTroops : TroopCount -> Bool
hasTroops (TroopCount troopCount) =
    troopCount > 0


noTroops : TroopCount
noTroops =
    TroopCount 0


subtractTroopCounts : TroopCount -> TroopCount -> TroopCount
subtractTroopCounts (TroopCount ammountToSubtract) (TroopCount subtractFrom) =
    TroopCount (subtractFrom - ammountToSubtract)


toString : TroopCount -> String
toString (TroopCount troopCount) =
    String.fromInt troopCount
