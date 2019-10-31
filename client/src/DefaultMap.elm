module DefaultMap exposing
    ( DefaultMap
    , Id
    , all
    )

import Map
import Maps.Big


type alias DefaultMap =
    { id : Id
    , map : Map.Map
    }


type Id
    = Id String


all : List DefaultMap
all =
    [ { id = Id "1", map = Map.parse "Big" Maps.Big.map }
    ]
