module Page.GameConfiguration exposing (Model, Msg, init, update, view)

import Color
import Dict
import Element
import Element.Background
import Element.Font
import Element.Input
import GameMap
import Html
import Html.Events
import Json.Decode
import Maps.Big
import Random
import Random.Dict
import Random.List
import TroopCount
import ViewHelpers


type alias GameConfiguration =
    { numberOfPlayers : String
    , windowSize :
        Maybe
            { width : Int
            , height : Int
            }
    }


type Model
    = ConfiguringGame GameConfiguration
    | GeneratingRandomTroopCounts GameConfiguration GameMap.GameMap


init : ( Model, Cmd Msg )
init =
    ( ConfiguringGame { numberOfPlayers = "2", windowSize = Nothing }, Cmd.none )


type Msg
    = NumberOfPlayersChanged String
    | StartGameClicked
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | NumberOfPlayersKeyPressed Int
    | WindowResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame gameConfiguration ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { gameConfiguration | numberOfPlayers = numberOfPlayers }, Cmd.none )

                StartGameClicked ->
                    startGame gameConfiguration

                NumberOfPlayersKeyPressed key ->
                    if key == 13 then
                        startGame gameConfiguration

                    else
                        ( model, Cmd.none )

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( ConfiguringGame { gameConfiguration | windowSize = Just { width = width, height = height } }, Cmd.none )

        GeneratingRandomTroopCounts gameConfiguration map ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    let
                        numberOfPlayers =
                            gameConfiguration.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    -- ( ActiveGame (ActiveGame.start map numberOfPlayers neutralCountryTroopCounts)
                    ( Debug.todo ""
                    , Cmd.none
                    )

                NumberOfPlayersChanged _ ->
                    ( model, Cmd.none )

                StartGameClicked ->
                    ( model, Cmd.none )

                NumberOfPlayersKeyPressed _ ->
                    ( model, Cmd.none )

                WindowResized width height ->
                    ( GeneratingRandomTroopCounts { gameConfiguration | windowSize = Just { width = width, height = height } } map, Cmd.none )


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


startGame : GameConfiguration -> ( Model, Cmd Msg )
startGame gameConfiguration =
    let
        map =
            GameMap.parse Maps.Big.map ViewHelpers.pixelsPerMapSquare
    in
    ( GeneratingRandomTroopCounts gameConfiguration map
    , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys map.countries))
    )


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String TroopCount.TroopCount)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        100
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (TroopCount.random maximumNeutralCountryTroops)



---- VIEW ----


toGameConfiguration : Model -> GameConfiguration
toGameConfiguration model =
    case model of
        ConfiguringGame gameConfiguration ->
            gameConfiguration

        GeneratingRandomTroopCounts gameConfiguration _ ->
            gameConfiguration


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { title = ""
    , content =
        Element.layout [ Element.width Element.fill, Element.centerX ]
            (Element.column
                [ Element.width Element.fill, Element.centerX ]
                [ Element.el
                    [ Element.padding 100
                    , Element.Font.bold
                    , Element.Font.size 80
                    , Element.centerX
                    , Element.Font.color (Color.darkBlue |> ViewHelpers.colorToElementColor)
                    ]
                    (Element.text "Fracas")
                , Element.el [ Element.centerX ]
                    (Element.column
                        [ Element.width Element.fill ]
                        [ Element.Input.text
                            [ Element.width (Element.px 50)
                            , Element.htmlAttribute
                                (Html.Events.on
                                    "keyup"
                                    (Json.Decode.map NumberOfPlayersKeyPressed Html.Events.keyCode)
                                )
                            ]
                            { onChange = NumberOfPlayersChanged
                            , text = model |> toGameConfiguration |> .numberOfPlayers
                            , placeholder = Nothing
                            , label =
                                Element.Input.labelLeft
                                    [ Element.centerY
                                    , Element.paddingEach { top = 0, left = 0, right = 10, bottom = 0 }
                                    ]
                                    (Element.text "Number of players")
                            }
                        , Element.Input.button
                            (ViewHelpers.defaultButtonAttributes
                                ++ [ Element.Background.color (Element.rgb255 0 150 0)
                                   , Element.width Element.fill
                                   , Element.padding 20
                                   , Element.centerX
                                   , Element.moveDown 30
                                   , Element.Font.size 30
                                   , Element.Font.color (Color.white |> ViewHelpers.colorToElementColor)
                                   ]
                            )
                            { onPress = Just StartGameClicked, label = ViewHelpers.centerText "Start Game" }
                        ]
                    )
                ]
            )
    }
