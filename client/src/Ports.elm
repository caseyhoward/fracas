port module Ports exposing
    ( createSubscriptions
    , gotSubscriptionData
    , socketStatusConnected
    , socketStatusReconnecting
    )

import Json.Decode


port gotSubscriptionData : (Json.Decode.Value -> msg) -> Sub msg


port socketStatusConnected : (() -> msg) -> Sub msg


port socketStatusReconnecting : (() -> msg) -> Sub msg


port createSubscriptions : String -> Cmd msg
