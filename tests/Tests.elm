module Tests exposing (..)

import Dict
import Expect
import Main
import Set
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


mapFile : String
mapFile =
    """
;Fracas 2.0 by Jason Merlo
;http://www.smozzie.com
;jmerlo@austin.rr.com
Created=9/11/2019 2:47:57 PM

;Terraform
CountrySize=150
CountryProportions=40
CountryShapes=44
MinLakeSize=6
LandPct=59
Islands=2

;Options
InitTroopPl=5
InitTroopCt=1
BonusTroops=2
Ships=3
Ports=3
Conquer=1
Events=1
1stTurn=2
HQSelect=2

;Preferences
Borders=5
Sound=1
AISpeed=2
AnimSpeed=1
Explosions=1
Waves=1
UnoccupiedColor=6
Flashing=1
Resolution=1
Prompt=1

;Players
Player1=Bowie,10,1,1
Player2=Pinky,8,2,6
Player3=Mac Dandy,11,2,6
Player4=Ozzie,2,2,6
Player5=Lady,4,2,6
Player6=Chula,3,2,6

{Menu and Map Data}
C:\\Program Files\\Fracas\\Maps\\simple.map
 22 
 1008 
{Map}
1.1.1000.
1.2.2.
3.2.4.
{Country Names}
Kirres
Thosko
Otivsica
Icren
    """


all : Test
all =
    describe "A Test Suite"
        [ test ".parseRawMap" <|
            \_ ->
                Expect.equal
                    (Main.parseRawMap mapFile)
                    (Dict.fromList
                        [ ( ( 0, 0 ), "3" )
                        , ( ( 0, 1 ), "1" )
                        , ( ( 0, 2 ), "1" )
                        , ( ( 1, 0 ), "2" )
                        , ( ( 1, 1 ), "2" )
                        , ( ( 1, 2 ), "1" )
                        , ( ( 2, 0 ), "4" )
                        , ( ( 2, 1 ), "2" )
                        , ( ( 2, 2 ), "1000" )
                        ]
                    )
        , test ".getNeighborCoordinates topLeft" <|
            \_ ->
                Expect.equal
                    (Main.getNeighborCoordinates ( 0, 0 ) ( 2, 2 ))
                    (Set.fromList [ ( 0, 1 ), ( 1, 0 ) ])
        , test ".getNeighborCoordinates bottomRight" <|
            \_ ->
                Expect.equal
                    (Main.getNeighborCoordinates ( 1, 1 ) ( 2, 2 ))
                    (Set.fromList [ ( 0, 1 ), ( 1, 0 ) ])
        , test ".getMapDimensions bottomRight" <|
            \_ ->
                Expect.equal
                    (Main.getMapDimensions
                        (Dict.fromList
                            [ ( ( 0, 0 ), "3" )
                            , ( ( 0, 1 ), "1" )
                            , ( ( 0, 2 ), "1" )
                            ]
                        )
                    )
                    ( 1, 3 )
        , test ".parseMap" <|
            \_ ->
                Expect.equal
                    (Main.parseMap mapFile)
                    { countries =
                        Dict.fromList
                            [ ( "1"
                              , { coordinates = Set.fromList [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]
                                , neighboringCountries = Set.fromList [ "2", "3" ]
                                , neighboringBodiesOfWater = Set.fromList [ "1000" ]
                                }
                              )
                            , ( "2"
                              , { coordinates = Set.fromList [ ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
                                , neighboringCountries = Set.fromList [ "1", "3", "4" ]
                                , neighboringBodiesOfWater = Set.fromList [ "1000" ]
                                }
                              )
                            , ( "3"
                              , { coordinates = Set.fromList [ ( 0, 0 ) ]
                                , neighboringCountries = Set.fromList [ "1", "2" ]
                                , neighboringBodiesOfWater = Set.fromList []
                                }
                              )
                            , ( "4"
                              , { coordinates = Set.fromList [ ( 2, 0 ) ]
                                , neighboringCountries = Set.fromList [ "2", "3" ]
                                , neighboringBodiesOfWater = Set.fromList [ "1000" ]
                                }
                              )
                            ]
                    , bodiesOfWater =
                        Dict.fromList
                            [ ( "4"
                              , { coordinates = Set.fromList [ ( 2, 2 ) ]
                                , neighboringCountries = Set.fromList [ "1", "2" ]
                                }
                              )
                            ]
                    }

        -- , test ".getEdges 1x1 1 scale" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.getEdgesForArea
        --                 (Set.fromList [ ( 0, 1 ) ])
        --                 1
        --             ).coordinates
        --             [ Main.BorderSegment ( 0, 2 ) ( 1, 2 )
        --             , Main.BorderSegment ( 0, 1 ) ( 1, 1 )
        --             , Main.BorderSegment ( 1, 1 ) ( 1, 2 )
        --             , Main.BorderSegment ( 0, 1 ) ( 0, 2 )
        --             ]
        -- , test ".getEdges 2x1 1 scale" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.getEdgesForArea
        --                 (Set.fromList [ ( 0, 1 ), ( 0, 2 ) ])
        --                 1
        --             ).coordinates
        --             [ Main.BorderSegment ( 0, 1 ) ( 1, 1 )
        --             , Main.BorderSegment ( 1, 1 ) ( 1, 2 )
        --             , Main.BorderSegment ( 0, 1 ) ( 0, 2 )
        --             , Main.BorderSegment ( 0, 3 ) ( 1, 3 )
        --             , Main.BorderSegment ( 1, 2 ) ( 1, 3 )
        --             , Main.BorderSegment ( 0, 2 ) ( 0, 3 )
        --             ]
        -- , test ".getEdges" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.getEdgesForArea
        --                 (Set.fromList [ ( 0, 1 ), ( 0, 2 ), ( 1, 1 ) ])
        --                 10
        --             ).coordinates
        --             [ Main.BorderSegment ( 0, 10 ) ( 10, 10 )
        --             , Main.BorderSegment ( 0, 10 ) ( 0, 20 )
        --             , Main.BorderSegment ( 0, 30 ) ( 10, 30 )
        --             , Main.BorderSegment ( 10, 20 ) ( 10, 30 )
        --             , Main.BorderSegment ( 0, 20 ) ( 0, 30 )
        --             , Main.BorderSegment ( 10, 20 ) ( 20, 20 )
        --             , Main.BorderSegment ( 10, 10 ) ( 20, 10 )
        --             , Main.BorderSegment ( 20, 10 ) ( 20, 20 )
        --             ]
        ]
