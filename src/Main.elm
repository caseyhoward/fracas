module Main exposing (..)

import Browser
import Collage exposing (circle, filled, rectangle, uniform)
import Collage.Layout
import Collage.Render
import Color
import Dict
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Set


type alias Country =
    { coordinates : Set.Set ( Int, Int )
    }


type alias GameMap =
    { countries : List Country
    }


defaultScale : Int
defaultScale =
    10



---- MODEL ----


type alias Model =
    ParsedMap



-- = PlayingGame PlayingGameAttributes
-- | NotPlayingGame
-- | GameFinished
-- type alias PlayingGameAttributes =
--     { unoccupiedCountries : List Country
--     , players : List Player
--     }
-- type alias Model2 =
--     { countries : List Country
--     , players : List Player
--     }
-- type alias Player =
--     { color : String
--     }
-- type Country
--     = Country
--         { name : String
--         , neighboringCountries : List Country
--         , bodiesOfWater : List BodyOfWater
--         }


init : ( Model, Cmd Msg )
init =
    ( parseMap mapFile, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ -- , h1 [] [ text "Your Elm App is working!" ]
          -- , text (Debug.toString model)
          renderMap model
        ]


renderMap : ParsedMap -> Html Msg
renderMap map =
    let
        edges =
            Dict.toList map
                |> List.head
                |> Maybe.map
                    (\( _, coordinates ) ->
                        getEdgesForCountry coordinates defaultScale
                    )

        rect =
            rectangle 200 100
                |> filled (uniform Color.blue)

        segments =
            case edges of
                Just e ->
                    e
                        |> Set.toList
                        |> Debug.log "asdf"
                        |> List.map (\( p1, p2 ) -> Collage.segment p1 p2)

                Nothing ->
                    []

        collages =
            List.map
                (\segment ->
                    Collage.traced Collage.defaultLineStyle segment
                )
                segments
    in
    rect
        |> Collage.Layout.at Collage.Layout.topLeft (Collage.group collages)
        |> Collage.Render.svg



-- let
--     circ =
--         circle 50
--             |> filled (uniform Color.red)
--     rect =
--         rectangle 200 100
--             |> filled (uniform Color.blue)
-- in
-- rect
--     |> Collage.Layout.at Collage.Layout.topLeft circ
--     |> Collage.Render.svg
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


type alias ParsedMap =
    Dict.Dict String (Set.Set ( Int, Int ))


parseMap : String -> ParsedMap
parseMap text =
    String.split "\n" text
        |> List.foldl
            (\row result ->
                case result of
                    ( countries, rowIndex ) ->
                        if rowIndex /= -1 then
                            if row /= "{Country Names}" then
                                ( parseCountryRow row rowIndex countries, rowIndex + 1 )

                            else
                                ( countries, -1 )

                        else if row == "{Map}" then
                            ( countries, 0 )

                        else
                            ( countries, -1 )
            )
            ( Dict.empty, -1 )
        |> Tuple.first


asdf : Set.Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Set.Set ( ( Int, Int ), ( Int, Int ) )
asdf coordinates adjacent original =
    if Set.member adjacent coordinates then
        Set.singleton ( original, adjacent )

    else
        Set.empty


getEdgesForCountry : Set.Set ( Int, Int ) -> Int -> Set.Set ( ( Float, Float ), ( Float, Float ) )
getEdgesForCountry allCoordinates scale =
    allCoordinates
        |> Set.foldl
            (\coordinate result ->
                Set.union result (getEdgesForCountryForCoordinate allCoordinates coordinate scale)
            )
            Set.empty


scaleCoordinate : Int -> ( Int, Int ) -> ( Float, Float )
scaleCoordinate scale ( x, y ) =
    ( x * scale |> toFloat, y * scale |> toFloat )


scaleEdge : Int -> ( ( Int, Int ), ( Int, Int ) ) -> ( ( Float, Float ), ( Float, Float ) )
scaleEdge scale ( point1, point2 ) =
    ( scaleCoordinate scale point1
    , scaleCoordinate scale point2
    )


getEdgesForCountryForCoordinate : Set.Set ( Int, Int ) -> ( Int, Int ) -> Int -> Set.Set ( ( Float, Float ), ( Float, Float ) )
getEdgesForCountryForCoordinate allCoordinates ( x, y ) scaleFactor =
    let
        left =
            ( x - 1, y )

        leftEdge =
            ( ( x, y ), ( x, y + 1 ) )

        right =
            ( x + 1, y )

        rightEdge =
            ( ( x + 1, y ), ( x + 1, y + 1 ) )

        above =
            ( x, y - 1 )

        aboveEdge =
            ( ( x, y ), ( x + 1, y ) )

        below =
            ( x, y + 1 )

        belowEdge =
            ( ( x, y + 1 ), ( x + 1, y + 1 ) )

        adjacentEdges =
            [ ( left, leftEdge )
            , ( right, rightEdge )
            , ( above, aboveEdge )
            , ( below, belowEdge )
            ]
    in
    adjacentEdges
        |> List.foldl
            (\( adjacent, edge ) result ->
                if Set.member adjacent allCoordinates then
                    result

                else
                    Set.insert (scaleEdge scaleFactor edge) result
            )
            Set.empty


parseCountryRow : String -> Int -> ParsedMap -> ParsedMap
parseCountryRow row rowIndex countries =
    String.split "." row
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> List.indexedMap Tuple.pair
        |> List.foldr
            (\( columnIndex, countryId ) result ->
                let
                    blah : ParsedMap
                    blah =
                        case Dict.get countryId result of
                            Just countryCoordinates ->
                                Dict.insert countryId (Set.insert ( columnIndex, rowIndex ) countryCoordinates) result

                            Nothing ->
                                Dict.insert countryId (Set.singleton ( columnIndex, rowIndex )) result
                in
                blah
            )
            countries



-- TODO
-- { countries =
--     [ { coordinates = [ ( 0, 0 ) ]
--       }
--     ]
-- }


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
19.19.19.19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.16.16.16.16.16.16.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.
19.19.19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.16.16.16.16.16.16.16.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.12.15.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.
19.19.19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.16.16.16.16.16.16.16.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.8.12.12.12.12.12.12.12.
19.19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.16.16.16.15.16.15.16.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.8.8.12.12.12.12.12.12.
19.19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.16.16.16.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.6.6.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.8.12.12.12.12.12.12.
19.19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.16.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.6.12.12.6.12.12.12.12.12.12.8.8.8.12.8.12.8.8.8.8.8.8.12.12.
19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.16.19.19.9.9.9.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.6.6.6.6.12.12.12.12.8.8.8.8.8.8.8.8.8.8.8.8.8.8.8.12.
19.19.19.19.19.19.19.19.19.19.19.16.16.16.16.19.19.19.19.19.9.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.15.6.6.6.12.12.12.12.8.8.8.8.8.8.8.8.8.8.8.8.8.8.8.8.
19.19.19.19.19.19.19.19.19.19.19.16.19.19.19.19.19.19.19.9.9.9.9.15.15.15.15.9.9.9.9.15.15.15.15.15.15.15.15.15.15.6.6.6.6.6.6.12.12.12.12.12.12.8.8.8.8.8.8.8.8.8.8.8.8.8.
19.19.19.19.19.19.19.19.19.19.19.19.19.19.19.14.19.19.19.9.9.9.9.9.9.15.9.9.9.9.9.9.9.15.15.15.9.9.6.6.6.6.6.6.6.6.6.6.6.6.6.12.6.8.8.8.8.8.8.8.8.8.8.8.8.8.
19.19.19.19.19.19.19.19.19.19.14.14.14.14.19.14.14.14.14.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.8.8.8.8.8.8.8.8.8.8.8.8.
19.19.19.19.19.19.19.19.14.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.8.8.8.8.8.8.8.8.8.8.8.8.
19.19.19.19.19.19.19.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.8.8.6.8.8.8.8.8.8.8.
19.19.19.19.19.19.19.19.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.1.1.1.9.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.8.6.6.6.8.8.8.8.8.8.
19.19.19.19.19.19.19.19.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.1.1.1.1.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.8.6.8.8.8.8.8.8.
19.19.19.19.19.19.19.14.14.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.9.9.9.9.9.1.1.1.1.1.1.1.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.8.8.8.8.8.8.8.8.8.
19.19.19.19.19.19.19.14.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.9.1.1.9.9.1.1.1.1.1.1.1.1.1.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.6.8.8.8.8.
19.19.19.21.21.21.22.22.14.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.1.1.1.1.1.1.1.1.1.1.1.1.1.1.6.1.6.6.6.6.6.6.6.6.6.6.6.6.4.6.6.6.6.8.6.6.8.8.8.8.
19.21.21.21.21.21.22.22.14.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.1.1.1.1.1.1.1.1.1.1.1.1.1.1.1.6.6.6.6.6.6.4.4.6.6.6.4.4.4.4.6.6.8.8.8.8.8.8.8.
21.21.21.21.22.22.22.22.22.14.14.14.14.14.14.14.14.14.14.9.9.9.9.9.9.9.9.1.17.1.1.1.1.1.1.1.1.1.1.1.1.1.6.6.6.6.6.6.6.4.4.4.4.4.4.4.4.4.4.4.8.8.8.8.8.8.
21.21.21.21.22.22.22.22.22.22.22.22.22.14.14.14.14.14.14.17.9.9.9.9.17.9.17.17.17.1.1.1.1.1.1.1.1.1.1.1.1.1.1.6.6.6.6.6.6.4.4.4.4.4.4.4.4.4.4.8.8.8.8.8.8.8.
21.21.21.21.22.22.22.22.22.22.22.22.22.14.14.14.14.14.17.17.9.9.9.17.17.9.17.17.1.1.1.1.1.1.1.1.1.1.1.1.3.1.1.6.6.6.6.6.6.6.3.4.4.4.4.4.4.18.18.18.8.8.1007.8.8.8.
21.22.22.22.22.22.22.22.22.22.22.22.22.14.14.14.14.14.17.17.17.17.9.17.17.17.17.17.1.1.1.1.1.1.1.1.1.1.1.1.3.3.3.3.6.6.6.6.6.3.3.4.4.4.4.4.4.18.18.18.18.1007.1007.1007.8.8.
21.22.22.22.22.22.22.22.22.22.22.22.14.14.14.14.14.17.17.17.17.17.17.17.17.17.17.1.1.1.1.1.1.1002.1.1.1.1.1.1.3.3.3.3.3.6.3.3.3.3.4.4.4.4.4.4.4.4.18.18.18.18.1007.1007.1007.1007.
22.22.22.22.22.22.22.22.22.22.22.22.14.14.14.14.17.17.17.17.17.17.17.17.17.10.10.1.1.1.1.1.1002.1002.1002.1.1.1.3.3.3.3.3.3.3.3.3.3.3.4.4.4.3.4.4.18.18.4.18.18.18.18.1007.1007.1007.1007.
22.22.22.22.22.22.22.22.22.22.22.22.14.14.14.14.17.17.17.17.17.17.10.10.10.10.10.10.1.1.1.1.1.1002.1002.1.1.1.1.1.1.3.3.3.3.3.3.3.3.3.4.3.3.3.4.4.18.18.18.18.18.18.1007.18.1007.1007.
22.22.22.22.22.22.22.22.22.22.22.22.22.14.14.17.17.17.17.17.17.17.17.10.10.10.10.10.1.1.1.1.1.1002.1002.1.1.1.1.3.3.3.3.3.3.3.3.3.3.3.3.3.3.18.4.18.18.18.18.18.18.18.18.18.1007.1007.
22.22.22.22.22.22.22.22.22.22.22.22.22.17.17.17.17.17.17.17.17.17.10.10.10.10.10.10.10.10.10.1.1.1.1.1.1.1.3.3.3.3.3.3.3.3.3.3.3.3.18.18.18.18.18.18.18.18.18.18.18.18.18.1007.1007.1007.
22.22.22.22.22.22.22.22.22.22.22.17.17.17.17.17.17.17.17.17.17.17.10.10.10.10.10.10.10.10.10.1.1.1.1.1.1.1.3.3.3.3.3.3.1004.1004.1004.1004.1004.3.18.18.18.18.18.7.18.18.18.18.18.18.18.1007.1007.1007.
22.22.22.22.22.22.22.22.22.22.22.17.17.17.17.17.17.17.17.17.17.10.10.10.10.10.10.10.10.10.1.1.1.1.1.1.3.3.3.3.3.3.3.3.1004.1004.1004.1004.3.3.18.7.7.7.18.7.18.18.18.7.7.7.7.1007.1007.1007.
22.22.22.22.22.22.22.22.22.22.22.20.20.17.17.17.17.17.10.10.10.10.10.10.10.10.10.10.2.10.1.1.1.1.3.3.3.3.3.3.3.3.3.3.1004.1004.1004.7.7.18.18.7.7.7.7.7.7.7.7.7.7.1007.1007.1007.1007.1007.
22.22.22.22.22.22.22.22.22.20.20.20.20.17.17.17.10.10.10.10.10.10.10.10.10.2.2.2.2.2.2.1.1.2.2.2.2.2.2.3.3.3.3.3.1004.1004.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.1007.1007.1007.
22.22.22.1001.1001.22.22.22.22.20.20.20.20.20.17.17.17.10.10.10.10.10.10.10.10.10.2.2.2.2.2.1.1.1.2.2.2.2.3.3.3.3.3.3.3.1004.1004.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.7.1007.1007.1007.
22.22.22.1001.1001.1001.22.22.22.20.20.20.20.20.20.17.17.10.10.10.10.10.10.10.10.10.2.2.2.2.2.1.2.2.2.2.2.2.3.2.3.3.3.3.3.1004.1004.3.7.7.7.7.7.7.1005.7.7.1005.1005.13.13.7.7.7.7.1007.
1001.1001.22.1001.1001.1001.1001.22.22.22.22.20.20.20.20.17.17.10.10.10.10.10.10.10.10.10.2.2.2.2.2.2.2.2.2.2.2.2.2.2.2.3.5.3.3.3.3.3.3.11.7.7.7.7.1005.1005.1005.1005.13.13.13.13.13.13.7.7.
1001.1001.1001.1001.22.22.22.22.1001.20.20.20.20.20.20.20.17.20.20.10.10.10.10.10.10.10.10.2.2.2.2.2.2.2.2.2.2.2.2.2.2.5.5.5.5.3.3.11.3.11.11.7.7.7.7.7.1005.1005.13.13.13.13.13.13.13.13.
1001.1001.1001.1001.22.1001.1001.1001.1001.1001.20.20.20.20.20.20.17.20.10.10.10.10.10.10.10.10.10.10.2.2.2.2.2.2.2.2.2.2.2.2.2.5.5.5.5.3.11.11.11.11.11.7.7.7.7.7.1005.13.13.13.13.13.13.13.13.13.
1001.1001.1001.1001.22.1001.1001.1001.1001.1001.20.20.20.20.20.17.17.20.20.10.10.10.10.10.20.10.2.10.2.2.2.2.2.2.2.2.2.2.2.2.2.5.5.5.5.3.3.11.11.11.11.7.11.7.7.13.13.13.13.13.13.13.13.13.13.13.
1001.1001.1001.1001.1001.1001.1001.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.2.2.2.2.2.2.2.2.2.2.2.2.2.2.2.2.2.1003.5.3.3.11.11.11.11.11.11.11.7.13.13.13.13.13.13.13.13.13.13.13.
1001.1001.1001.1001.1001.1001.1001.1001.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.2.2.2.2.2.2.2.2.2.2.2.2.2.2.1003.1003.5.5.3.3.3.11.11.11.11.11.11.7.11.11.13.13.13.13.13.13.13.13.13.
1001.1001.1001.1001.1001.1001.1001.1001.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.2.2.2.2.2.2.2.2.2.2.2.2.2.2.1003.1003.5.5.5.11.11.11.11.11.11.11.11.11.11.11.13.13.13.1006.13.13.13.13.13.
1001.1001.1001.1001.1001.1001.1001.1001.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.2.2.2.2.2.2.2.2.1003.2.2.2.2.1003.1003.1003.5.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.1006.13.13.13.13.
1001.1001.1001.1001.1001.1001.1001.1001.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.2.2.2.2.2.2.2.2.1003.2.1003.2.2.2.1003.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.1006.1006.1006.11.1006.1006.1006.13.13.
1001.1001.1001.1001.1001.1001.1001.1001.1001.1001.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.2.20.2.2.1003.1003.1003.1003.1003.1003.1003.1003.1003.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.1006.1006.1006.1006.1006.13.13.13.
1001.1001.1001.1001.1001.1001.1001.1001.1001.1001.1001.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.20.2.1003.1003.1003.1003.1003.1003.1003.1003.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.1006.1006.1006.1006.1006.1006.1006.1006.
{Country Names}
Kirres
Thosko
Otivsica
Icren
Ziland
Heland
Ento
Ucsary
Ryli
Vritia
Dygarica
Hure
Vrobrica
Inaway
North Bror
Utania
Vraburg
Ibro
Cloton
Leburg
East Seggany
Juiburg
{Water Names}
The Bay of Kinni
Vrud Sea
The Bay of Assar
Howseton Sea
Sniland Vista
Praiffary Harbor
Pler Ocean
Kletwi Lake
{Hi Scores}
HI1=Lady,91,0
HI2=BooBoo,82,0
HI3=Mac,78,0
HI4=Bowie,66,0
HI5=Pinky,58,0
HI6=Queenie,43,0
HI7=Zack,35,0
HI8=Smudge,26,0
HI9=Ozzie,15,0
HI10=Penny,5,0
    """
