module Main exposing (main)

import ActiveGame
import Browser
import Collage
import Collage.Events
import Collage.Render
import Collage.Text
import Color
import Dict
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import GameMap
import Html exposing (Html)
import Maps.Big
import Random
import Random.Dict
import Random.List
import Set
import TroopCount



-- Settings


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


countryBorderColor : Color.Color
countryBorderColor =
    Color.black



---- MODEL ----


type Model
    = ConfiguringGame ConfigurationAttributes
    | PlayingGame ActiveGame.ActiveGame
    | GeneratingRandomTroopCounts ConfigurationAttributes GameMap.GameMap


type alias ConfigurationAttributes =
    { numberOfPlayers : String }


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( ConfiguringGame { numberOfPlayers = "2" }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = CountryClicked GameMap.CountryId
    | NumberOfPlayersChanged String
    | StartGameClicked
    | Pass
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | UpdateNumberOfTroopsToMove String
    | CancelMovingTroops


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame configurationOptions ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { configurationOptions | numberOfPlayers = numberOfPlayers }, Cmd.none )

                StartGameClicked ->
                    let
                        map =
                            GameMap.parse Maps.Big.map ActiveGame.defaultScale
                    in
                    ( GeneratingRandomTroopCounts configurationOptions map
                    , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys map.countries))
                    )

                Pass ->
                    ( model, Cmd.none )

                CountryClicked _ ->
                    ( model, Cmd.none )

                UpdateNumberOfTroopsToMove _ ->
                    ( model, Cmd.none )

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                CancelMovingTroops ->
                    ( model, Cmd.none )

        GeneratingRandomTroopCounts configurationOptions map ->
            case msg of
                NeutralCountryTroopCountsGenerated neutralCountryTroopCounts ->
                    let
                        numberOfPlayers =
                            configurationOptions.numberOfPlayers
                                |> String.toInt
                                |> Maybe.withDefault 6
                    in
                    ( ActiveGame.start map numberOfPlayers neutralCountryTroopCounts |> PlayingGame, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PlayingGame attributes ->
            case msg of
                CountryClicked clickedCountryId ->
                    ( PlayingGame (ActiveGame.handleCountryClickFromPlayer clickedCountryId attributes)
                    , Cmd.none
                    )

                Pass ->
                    ( ActiveGame.pass attributes |> PlayingGame, Cmd.none )

                NumberOfPlayersChanged _ ->
                    ( model, Cmd.none )

                StartGameClicked ->
                    ( model, Cmd.none )

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                UpdateNumberOfTroopsToMove numberOfTroopsToMoveString ->
                    ( attributes |> ActiveGame.updateNumberOfTroopsToMove numberOfTroopsToMoveString |> PlayingGame, Cmd.none )

                CancelMovingTroops ->
                    ( attributes |> ActiveGame.cancelMovingTroops |> PlayingGame, Cmd.none )


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String TroopCount.TroopCount)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        100
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (TroopCount.random maximumNeutralCountryTroops)



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        ConfiguringGame configuringGameSettings ->
            viewGameConfiguration configuringGameSettings

        GeneratingRandomTroopCounts _ _ ->
            Element.layout [] Element.none

        PlayingGame activeGame ->
            viewPlayingGame activeGame



---- Configuration


viewGameConfiguration : ConfigurationAttributes -> Html Msg
viewGameConfiguration gameConfiguration =
    Element.layout [ Element.width Element.fill ]
        (Element.column
            [ Element.width Element.fill, Element.centerX ]
            [ Element.el [ Element.centerX ]
                (Element.row
                    [ Element.width Element.fill ]
                    [ Element.Input.text
                        []
                        { onChange = NumberOfPlayersChanged
                        , text = gameConfiguration.numberOfPlayers
                        , placeholder = Nothing
                        , label = Element.Input.labelLeft [ Element.centerY ] (Element.text "Number of players")
                        }
                    , Element.Input.button (defaultButtonAttributes ++ [ Element.Background.color (Element.rgb255 0 150 0) ]) { onPress = Just StartGameClicked, label = Element.text "Start Game" }
                    ]
                )
            ]
        )



---- PlayingGame


infoPanelWidth =
    200


viewPlayingGame : ActiveGame.ActiveGame -> Html Msg
viewPlayingGame activeGame =
    Element.layout [ Element.width Element.fill ]
        (Element.row
            [ Element.centerX ]
            [ viewInfoPanel activeGame
            , Element.column
                [ Element.centerX ]
                ([ Element.el [ Element.centerX, Element.width Element.fill ]
                    (renderGameBoard activeGame |> Element.html)
                 ]
                    ++ (case activeGame.error of
                            Just error ->
                                [ Element.text error ]

                            Nothing ->
                                []
                       )
                    ++ [ viewPlayerTurnStatus activeGame.currentPlayerTurn activeGame.players ]
                )
            ]
        )


viewInfoPanel : ActiveGame.ActiveGame -> Element.Element Msg
viewInfoPanel activeGame =
    Element.column
        [ Element.width (Element.px 200)
        , Element.height Element.fill
        , Element.alignTop
        , Element.Border.width 1
        , Element.Border.color black
        , Element.Border.solid
        ]
        [ viewPassButtonIfNecessary activeGame
        , viewConfigureTroopCountIfNecessary activeGame
        ]


viewPassButtonIfNecessary : ActiveGame.ActiveGame -> Element.Element Msg
viewPassButtonIfNecessary activeGame =
    Element.el
        [ Element.height (Element.px 80)
        , Element.width Element.fill
        , Element.moveDown 20

        -- , Element.explain Debug.todo
        ]
        (if ActiveGame.canCurrentPlayerPass activeGame then
            Element.Input.button
                (defaultButtonAttributes
                    ++ [ Element.width (Element.px 120)
                       , Element.centerX

                       --    , Element.paddingXY 0 10
                       , 40 |> Element.px |> Element.height
                       , Element.Background.color (Element.rgb255 100 200 100)
                       ]
                )
                { onPress = Just Pass, label = Element.text "Pass" }

         else
            Element.none
        )


black : Element.Color
black =
    Element.rgb255 0 100 100


defaultButtonAttributes : List (Element.Attribute msg)
defaultButtonAttributes =
    [ Element.padding 10
    , Element.Background.color (Element.rgb255 200 200 200)
    , Element.Font.color (Element.rgb255 0 0 0)
    , Element.Font.size 16
    , Element.Font.bold
    , Element.Border.rounded 2
    , Element.Border.shadow { offset = ( 2, 2 ), size = 1, blur = 1, color = Element.rgba 0 0 0 0.1 }
    , Element.width Element.fill
    , Element.Font.variant Element.Font.smallCaps
    ]


defaultTextInputAttributes : List (Element.Attribute msg)
defaultTextInputAttributes =
    [ Element.Border.width 1
    , Element.width Element.fill
    , Element.height Element.fill
    , Element.Font.size 16
    , Element.Border.rounded 2
    , Element.padding 15
    , Element.Border.color (Element.rgb255 100 100 100)
    , Element.Border.solid
    ]


defaultLabelAttributes : List (Element.Attribute msg)
defaultLabelAttributes =
    [ Element.Font.size 12

    -- , Element.moveUp 7
    -- , Element.moveRight 5
    -- , Element.paddingEach { left = 5, top = 0, right = 5, bottom = 0 }
    ]


viewConfigureTroopCountIfNecessary : ActiveGame.ActiveGame -> Element.Element Msg
viewConfigureTroopCountIfNecessary activeGame =
    Element.el
        [ 100 |> Element.px |> Element.height
        , Element.explain Debug.todo
        ]
        (case activeGame |> ActiveGame.troopToMove of
            Just numberOfTroopsToMove ->
                Element.column
                    []
                    [ Element.Input.text
                        (defaultTextInputAttributes ++ [ Element.alignLeft ])
                        { onChange = UpdateNumberOfTroopsToMove
                        , placeholder = Nothing
                        , label = Element.Input.labelAbove defaultLabelAttributes (Element.text "Number of troops to move")
                        , text = numberOfTroopsToMove
                        }
                    , Element.Input.button
                        (defaultButtonAttributes
                            ++ [ Element.width (Element.px 100)
                               , Element.centerX
                               , Element.Font.color (Element.rgb255 255 255 255)
                               , Element.Background.color (Element.rgb255 255 63 63)
                               ]
                        )
                        { onPress = Just CancelMovingTroops, label = Element.text "Cancel" }
                    ]

            Nothing ->
                Element.none
        )


viewPlayerTurnStatus : ActiveGame.PlayerTurn -> Dict.Dict Int ActiveGame.Player -> Element.Element Msg
viewPlayerTurnStatus playerTurn players =
    Element.el [ Element.width Element.fill, Element.Background.color (ActiveGame.getPlayerColorFromPlayerTurn players playerTurn |> colorToElementColor), Element.padding 5 ]
        (Element.text
            (ActiveGame.playerTurnToString players playerTurn)
        )


colorToElementColor : Color.Color -> Element.Color
colorToElementColor color =
    color |> Color.toRgba |> Element.fromRgb



-- Rendering


renderGameBoard : ActiveGame.ActiveGame -> Html Msg
renderGameBoard activeGame =
    let
        countryCollages : List (Collage.Collage Msg)
        countryCollages =
            activeGame.map.countries
                |> GameMap.getCountryIds
                |> List.map
                    (\countryId -> renderCountry countryId activeGame)

        background =
            Collage.polygon
                [ ( 0, 0 )
                , ( 0, activeGame.map.dimensions |> Tuple.second )
                , ( activeGame.map.dimensions |> Tuple.first, activeGame.map.dimensions |> Tuple.second )
                , ( activeGame.map.dimensions |> Tuple.first, 0 )
                ]

        backgroundWater =
            background
                |> Collage.filled (Collage.uniform Color.blue)

        backgroundBorder =
            background
                |> Collage.outlined (Collage.solid (toFloat ActiveGame.defaultScale / 8.0) (Collage.uniform Color.black))
    in
    Collage.group (countryCollages ++ [ backgroundBorder, backgroundWater ])
        |> Collage.Render.svg


renderCountry : GameMap.CountryId -> ActiveGame.ActiveGame -> Collage.Collage Msg
renderCountry countryId activeGame =
    case ( ActiveGame.findCountryOwner countryId activeGame.players, GameMap.getCountry countryId activeGame.map.countries ) of
        ( Just countryOwnerId, Just country ) ->
            case
                ( ActiveGame.getPlayer countryOwnerId activeGame.players
                , ActiveGame.getTroopCountForPlayerCountry countryId countryOwnerId activeGame.players
                , ActiveGame.getCountryHasPort countryOwnerId countryId activeGame.players
                )
            of
                ( Just player, Just troopCount, Just hasPort ) ->
                    Collage.group
                        ((if hasPort then
                            [ renderPort country.waterEdges ]

                          else
                            []
                         )
                            ++ [ renderTroopCount country.center troopCount
                               , renderArea country.polygon player.color player.capitolStatus countryId
                               ]
                        )
                        |> Collage.Events.onClick (CountryClicked countryId)

                _ ->
                    Collage.Text.fromString "Error rendering country 1" |> Collage.rendered

        ( Nothing, Just country ) ->
            case ActiveGame.getTroopCount countryId activeGame.neutralCountryTroops of
                Just troopCount ->
                    Collage.group
                        [ renderTroopCount country.center troopCount
                        , renderArea country.polygon Color.gray ActiveGame.NoCapitol countryId
                        ]
                        |> Collage.Events.onClick (CountryClicked countryId)

                _ ->
                    Collage.group
                        [ renderArea country.polygon Color.gray ActiveGame.NoCapitol countryId
                        ]
                        |> Collage.Events.onClick (CountryClicked countryId)

        _ ->
            Collage.Text.fromString "Error rendering country 4" |> Collage.rendered


renderPort : Set.Set GameMap.BorderSegment -> Collage.Collage msg
renderPort waterEdges =
    waterEdges
        |> Set.toList
        |> List.map
            (\( point1, point2 ) ->
                Collage.segment point1 point2
                    |> Collage.traced
                        (Collage.broken [ ( 3, 10 ) ]
                            ((ActiveGame.defaultScale |> toFloat) / 2.0)
                            (Collage.uniform Color.gray)
                        )
            )
        |> Collage.group


renderTroopCount : ( Int, Int ) -> TroopCount.TroopCount -> Collage.Collage msg
renderTroopCount ( medianX, medianY ) troopCount =
    if TroopCount.hasTroops troopCount then
        troopCount
            |> TroopCount.toString
            |> Collage.Text.fromString
            |> Collage.Text.color Color.black
            |> Collage.Text.size (ActiveGame.defaultScale * 100 // 120)
            |> Collage.rendered
            |> Collage.shift ( (toFloat medianX + 0.5) * toFloat ActiveGame.defaultScale, (toFloat medianY + 0.5) * toFloat ActiveGame.defaultScale )

    else
        Collage.group []


renderArea : List ( Float, Float ) -> Color.Color -> ActiveGame.CapitolStatus -> GameMap.CountryId -> Collage.Collage msg
renderArea polygonPoints color capitolStatus countryId =
    let
        scale =
            ActiveGame.defaultScale

        ( capitolDot, capitolDotsCoords ) =
            case capitolStatus of
                ActiveGame.Capitol capitolId coords ->
                    if countryId == capitolId then
                        ( [ Collage.square (toFloat scale / 10.0)
                                |> Collage.filled (Collage.uniform Color.black)
                          ]
                        , coords
                        )

                    else
                        ( [], Set.empty )

                ActiveGame.NoCapitol ->
                    ( [], Set.empty )

        renderedDot =
            capitolDot
                |> Collage.group

        polygon =
            Collage.polygon polygonPoints

        polygonBorder =
            polygon
                |> Collage.outlined (Collage.solid (toFloat ActiveGame.defaultScale / 24.0) (Collage.uniform countryBorderColor))

        polygonFill =
            polygon
                |> Collage.filled (Collage.uniform color)

        drawnDots =
            capitolDotsCoords
                |> Set.foldl
                    (\coordinates result ->
                        (renderedDot |> Collage.shift coordinates) :: result
                    )
                    []
    in
    Collage.group (drawnDots ++ [ polygonBorder, polygonFill ])



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
