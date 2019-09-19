module ActiveGameTests exposing (..)

import ActiveGame
import Color
import Dict
import Expect
import Set
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "ActiveGame"
        [ test ".start" <|
            \_ ->
                Expect.equal True True
        ]
