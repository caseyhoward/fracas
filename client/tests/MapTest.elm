module MapTest exposing (..)

import Expect
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Game"
        [ test ".start" <|
            \_ ->
                Expect.equal True True
        ]
