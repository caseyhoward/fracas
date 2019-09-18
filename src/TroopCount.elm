module TroopCount exposing (TroopCount(..), addTroopCounts, hasTroops, noTroops, nullTroopCount, subtractTroopCounts)


type TroopCount
    = TroopCount Int


nullTroopCount : TroopCount
nullTroopCount =
    TroopCount -1


noTroops : TroopCount
noTroops =
    TroopCount 0


addTroopCounts : TroopCount -> TroopCount -> TroopCount
addTroopCounts (TroopCount troopCount1) (TroopCount troopCount2) =
    TroopCount (troopCount1 + troopCount2)


subtractTroopCounts : TroopCount -> TroopCount -> TroopCount
subtractTroopCounts (TroopCount ammountToSubtract) (TroopCount subtractFrom) =
    TroopCount (subtractFrom - ammountToSubtract)


hasTroops : TroopCount -> Bool
hasTroops (TroopCount troopCount) =
    troopCount > 0
