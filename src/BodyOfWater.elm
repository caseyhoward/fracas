module BodyOfWater exposing (Id, idToString)


type Id
    = Id String


idToString : Id -> String
idToString (Id id) =
    id
