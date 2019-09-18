module TroopCount exposing (TroopCount(..), noTroops, nullTroopCount)


type TroopCount
    = TroopCount Int


nullTroopCount : TroopCount
nullTroopCount =
    TroopCount -1


noTroops : TroopCount
noTroops =
    TroopCount 0
