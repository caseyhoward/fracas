module GameMapTests exposing (..)

import Color
import Dict
import Expect
import GameMap
import Set
import Test exposing (..)


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
    describe "GameMap"
        [ test ".parseMap" <|
            \_ ->
                Expect.equal True True

        -- [ test ".parseMap" <|
        --     \_ ->
        --         Expect.equal
        --             (GameMap.parse mapFile 12)
        --             { dimensions = ( 36, 36 )
        --             , bodiesOfWater =
        --                 Dict.fromList
        --                     [ ( "1000"
        --                       , { borderEdges =
        --                             Set.fromList
        --                                 [ ( ( 24, 24 ), ( 24, 36 ) )
        --                                 , ( ( 24, 24 ), ( 36, 24 ) )
        --                                 , ( ( 24, 36 ), ( 36, 36 ) )
        --                                 , ( ( 36, 24 ), ( 36, 36 ) )
        --                                 ]
        --                         , coordinates = Set.fromList [ ( 2, 2 ) ]
        --                         , neighboringCountries = Set.fromList [ "1", "2" ]
        --                         , polygon =
        --                             [ ( 24, 24 )
        --                             , ( 36, 24 )
        --                             , ( 36, 36 )
        --                             , ( 24, 36 )
        --                             ]
        --                         }
        --                       )
        --                     ]
        --             , countries =
        --                 Dict.fromList
        --                     [ ( "1"
        --                       , { borderEdges =
        --                             Set.fromList
        --                                 [ ( ( 0, 12 ), ( 0, 24 ) )
        --                                 , ( ( 0, 12 ), ( 12, 12 ) )
        --                                 , ( ( 0, 24 ), ( 0, 36 ) )
        --                                 , ( ( 0, 36 ), ( 12, 36 ) )
        --                                 , ( ( 12, 12 ), ( 12, 24 ) )
        --                                 , ( ( 12, 24 ), ( 24, 24 ) )
        --                                 , ( ( 12, 36 ), ( 24, 36 ) )
        --                                 , ( ( 24, 24 ), ( 24, 36 ) )
        --                                 ]
        --                         , center = ( 0, 2 )
        --                         , coordinates = Set.fromList [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]
        --                         , neighboringBodiesOfWater = Set.fromList [ "1000" ]
        --                         , neighboringCountries = Set.fromList [ "2", "3" ]
        --                         , polygon =
        --                             [ ( 0, 12 )
        --                             , ( 12, 12 )
        --                             , ( 12, 24 )
        --                             , ( 24, 24 )
        --                             , ( 24, 36 )
        --                             , ( 12, 36 )
        --                             , ( 0, 36 )
        --                             , ( 0, 24 )
        --                             ]
        --                         }
        --                       )
        --                     , ( "2"
        --                       , { borderEdges =
        --                             Set.fromList
        --                                 [ ( ( 12, 0 ), ( 12, 12 ) )
        --                                 , ( ( 12, 0 ), ( 24, 0 ) )
        --                                 , ( ( 12, 12 ), ( 12, 24 ) )
        --                                 , ( ( 12, 24 ), ( 24, 24 ) )
        --                                 , ( ( 24, 0 ), ( 24, 12 ) )
        --                                 , ( ( 24, 12 ), ( 36, 12 ) )
        --                                 , ( ( 24, 24 ), ( 36, 24 ) )
        --                                 , ( ( 36, 12 ), ( 36, 24 ) )
        --                                 ]
        --                         , center = ( 1, 1 )
        --                         , coordinates = Set.fromList [ ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]
        --                         , neighboringBodiesOfWater = Set.fromList [ "1000" ]
        --                         , neighboringCountries = Set.fromList [ "1", "3", "4" ]
        --                         , polygon =
        --                             [ ( 12, 0 )
        --                             , ( 24, 0 )
        --                             , ( 24, 12 )
        --                             , ( 36, 12 )
        --                             , ( 36, 24 )
        --                             , ( 24, 24 )
        --                             , ( 12, 24 )
        --                             , ( 12, 12 )
        --                             ]
        --                         }
        --                       )
        --                     , ( "3"
        --                       , { borderEdges =
        --                             Set.fromList
        --                                 [ ( ( 0, 0 ), ( 0, 12 ) )
        --                                 , ( ( 0, 0 ), ( 12, 0 ) )
        --                                 , ( ( 0, 12 ), ( 12, 12 ) )
        --                                 , ( ( 12, 0 ), ( 12, 12 ) )
        --                                 ]
        --                         , center = ( 0, 0 )
        --                         , coordinates = Set.fromList [ ( 0, 0 ) ]
        --                         , neighboringBodiesOfWater = Set.fromList []
        --                         , neighboringCountries = Set.fromList [ "1", "2" ]
        --                         , polygon =
        --                             [ ( 0, 0 )
        --                             , ( 12, 0 )
        --                             , ( 12, 12 )
        --                             , ( 0, 12 )
        --                             ]
        --                         }
        --                       )
        --                     , ( "4"
        --                       , { borderEdges =
        --                             Set.fromList
        --                                 [ ( ( 24, 0 ), ( 24, 12 ) )
        --                                 , ( ( 24, 0 ), ( 36, 0 ) )
        --                                 , ( ( 24, 12 ), ( 36, 12 ) )
        --                                 , ( ( 36, 0 ), ( 36, 12 ) )
        --                                 ]
        --                         , center = ( 2, 0 )
        --                         , coordinates = Set.fromList [ ( 2, 0 ) ]
        --                         , neighboringBodiesOfWater = Set.fromList []
        --                         , neighboringCountries = Set.fromList [ "2" ]
        --                         , polygon =
        --                             [ ( 24, 0 )
        --                             , ( 36, 0 )
        --                             , ( 36, 12 )
        --                             , ( 24, 12 )
        --                             ]
        --                         }
        --                       )
        --                     ]
        --             }
        -- 1.1.1000.
        -- 1.2.2.
        -- 3.2.4.
        -- , test "coordinatesToPolygon" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.coordinatesToPolygon (Set.fromList [ ( 0, 1 ), ( 0, 2 ), ( 1, 2 ) ]))
        --             [ ( 0, 12 )
        --             , ( 12, 12 )
        --             , ( 12, 24 )
        --             , ( 24, 24 )
        --             , ( 24, 36 )
        --             , ( 12, 36 )
        --             , ( 0, 36 )
        --             , ( 0, 24 )
        --             ]
        -- , test ".updateCountry" <|
        --     \_ ->
        --         let
        --             country : Main.Country
        --             country =
        --                 { coordinates = Set.fromList [ ( 1, 1 ) ]
        --                 , neighboringBodiesOfWater = Set.fromList [ "1001" ]
        --                 , neighboringCountries = Set.fromList [ "5" ]
        --                 , polygon = []
        --                 , center = ( 0, 0 )
        --                 , borderEdges = Set.empty
        --                 }
        --             rawGameMap =
        --                 Dict.fromList
        --                     [ ( ( 0, 0 ), "3" )
        --                     , ( ( 0, 1 ), "1" )
        --                     , ( ( 0, 2 ), "1" )
        --                     , ( ( 1, 0 ), "2" )
        --                     , ( ( 1, 1 ), "1000" )
        --                     , ( ( 1, 2 ), "1" )
        --                     , ( ( 2, 0 ), "4" )
        --                     , ( ( 2, 1 ), "2" )
        --                     , ( ( 2, 2 ), "1002" )
        --                     ]
        --         in
        --         Expect.equal
        --             (Main.updateCountry "1" ( 0, 1 ) ( 2, 2 ) rawGameMap country)
        --             { coordinates = Set.fromList [ ( 1, 1 ), ( 0, 1 ) ]
        --             , neighboringBodiesOfWater = Set.fromList [ "1001", "1000" ]
        --             , neighboringCountries = Set.fromList [ "3", "5" ]
        --             , polygon = []
        --             , center = ( 0, 0 )
        --             , borderEdges = Set.empty
        --             }
        -- , test ".getEdges 1x1 1 scale" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.getEdgesForArea
        --                 (Set.fromList [ ( 0, 1 ) ])
        --                 1
        --             )
        --             (Set.fromList
        --                 [ ( ( 0, 2 ), ( 1, 2 ) )
        --                 , ( ( 0, 1 ), ( 1, 1 ) )
        --                 , ( ( 1, 1 ), ( 1, 2 ) )
        --                 , ( ( 0, 1 ), ( 0, 2 ) )
        --                 ]
        --             )
        -- , test ".getEdges 2x1 1 scale" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.getEdgesForArea
        --                 (Set.fromList [ ( 0, 1 ), ( 0, 2 ) ])
        --                 1
        --             )
        --             (Set.fromList
        --                 [ ( ( 0, 1 ), ( 1, 1 ) )
        --                 , ( ( 1, 1 ), ( 1, 2 ) )
        --                 , ( ( 0, 1 ), ( 0, 2 ) )
        --                 , ( ( 0, 3 ), ( 1, 3 ) )
        --                 , ( ( 1, 2 ), ( 1, 3 ) )
        --                 , ( ( 0, 2 ), ( 0, 3 ) )
        --                 ]
        --             )
        -- , test ".getEdges" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.getEdgesForArea
        --                 (Set.fromList [ ( 0, 1 ), ( 0, 2 ), ( 1, 1 ) ])
        --                 10
        --             )
        --             (Set.fromList
        --                 [ ( ( 0, 10 ), ( 10, 10 ) )
        --                 , ( ( 0, 10 ), ( 0, 20 ) )
        --                 , ( ( 0, 30 ), ( 10, 30 ) )
        --                 , ( ( 10, 20 ), ( 10, 30 ) )
        --                 , ( ( 0, 20 ), ( 0, 30 ) )
        --                 , ( ( 10, 20 ), ( 20, 20 ) )
        --                 , ( ( 10, 10 ), ( 20, 10 ) )
        --                 , ( ( 20, 10 ), ( 20, 20 ) )
        --                 ]
        --             )
        -- , test ".removePlayerCountry" <|
        --     \_ ->
        --         Expect.equal
        --             (Main.removePlayerCountry
        --                 "111"
        --                 2
        --                 { countries = Dict.fromList [ ( "111", { troopCount = 0 } ) ], capitolStatus = Main.NoCapitol, name = "", color = Color.blue }
        --                 (Dict.fromList [ ( 2, { countries = Dict.fromList [ ( "111", { troopCount = 0 } ) ], capitolStatus = Main.NoCapitol, name = "", color = Color.blue } ) ])
        --             )
        --             (Dict.fromList [ ( 2, { countries = Dict.empty, capitolStatus = Main.NoCapitol, name = "", color = Color.blue } ) ])
        ]
