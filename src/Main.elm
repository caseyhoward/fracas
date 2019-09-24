module Main exposing (main)

-- import Maps.Big

import ActiveGame
import Browser
import Browser.Dom
import Browser.Events
import Collage
import Collage.Events
import Collage.Layout
import Collage.Render
import Collage.Text
import Color
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import GameMap
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Maps.Big
-- import Maps.UnitedStates
import Random
import Random.Dict
import Random.List
import Set
import Task
import Time
import TroopCount



-- Settings


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


countryBorderColor : Color.Color
countryBorderColor =
    Color.rgb255 100 100 100


countryOutlineDelayMilliseconds : Float
countryOutlineDelayMilliseconds =
    300



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
    = CountryMouseUp GameMap.CountryId
    | CountryMouseDown GameMap.CountryId
    | CountryMouseOut GameMap.CountryId
    | MouseUp
    | NumberOfPlayersChanged String
    | StartGameClicked
    | Pass
    | NeutralCountryTroopCountsGenerated (Dict.Dict String TroopCount.TroopCount)
    | UpdateNumberOfTroopsToMove String
    | CancelMovingTroops
    | NumberOfPlayersKeyPressed Int
    | ShowCountryBorderHelper
    | ShowAvailableMovesCheckboxToggled Bool
    | WindowResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        ConfiguringGame configurationOptions ->
            case msg of
                NumberOfPlayersChanged numberOfPlayers ->
                    ( ConfiguringGame { configurationOptions | numberOfPlayers = numberOfPlayers }, Cmd.none )

                StartGameClicked ->
                    startGame configurationOptions

                NumberOfPlayersKeyPressed key ->
                    if key == 13 then
                        startGame configurationOptions

                    else
                        ( model, Cmd.none )

                Pass ->
                    ( model, Cmd.none )

                CountryMouseUp _ ->
                    ( model, Cmd.none )

                CountryMouseOut _ ->
                    ( model, Cmd.none )

                CountryMouseDown _ ->
                    ( model, Cmd.none )

                UpdateNumberOfTroopsToMove _ ->
                    ( model, Cmd.none )

                NeutralCountryTroopCountsGenerated _ ->
                    ( model, Cmd.none )

                CancelMovingTroops ->
                    ( model, Cmd.none )

                ShowCountryBorderHelper ->
                    ( model, Cmd.none )

                MouseUp ->
                    ( model, Cmd.none )

                ShowAvailableMovesCheckboxToggled _ ->
                    ( model, Cmd.none )

                WindowResized _ _ ->
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
                    ( ActiveGame.start map numberOfPlayers neutralCountryTroopCounts |> PlayingGame
                    , Task.attempt
                        (\viewportResult ->
                            case viewportResult of
                                Ok viewport ->
                                    WindowResized (round viewport.viewport.width) (round viewport.viewport.height)

                                Err _ ->
                                    WindowResized 0 0
                        )
                        Browser.Dom.getViewport
                    )

                _ ->
                    ( model, Cmd.none )

        PlayingGame attributes ->
            case msg of
                CountryMouseUp clickedCountryId ->
                    ( PlayingGame (ActiveGame.handleCountryMouseUpFromPlayer clickedCountryId attributes)
                    , Cmd.none
                    )

                CountryMouseDown clickedCountryId ->
                    ( PlayingGame (ActiveGame.handleCountryMouseDown clickedCountryId attributes), Cmd.none )

                CountryMouseOut mouseOutCountryId ->
                    ( PlayingGame (ActiveGame.handleCountryMouseOut mouseOutCountryId attributes), Cmd.none )

                ShowAvailableMovesCheckboxToggled isChecked ->
                    ( PlayingGame { attributes | showAvailableMoves = isChecked }, Cmd.none )

                MouseUp ->
                    ( PlayingGame (ActiveGame.stopShowingCountryHelperOutlines attributes), Cmd.none )

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

                NumberOfPlayersKeyPressed _ ->
                    ( model, Cmd.none )

                ShowCountryBorderHelper ->
                    ( PlayingGame (ActiveGame.makeCountryHelperOutlinesActive attributes), Cmd.none )

                WindowResized width height ->
                    ( PlayingGame (ActiveGame.setWindowSize width height attributes), Cmd.none )


randomTroopPlacementsGenerator : List String -> Random.Generator (Dict.Dict String TroopCount.TroopCount)
randomTroopPlacementsGenerator countryIds =
    -- This can pick the same country twice so you might not get the max number of countries
    Random.Dict.dict
        100
        (Random.List.choose countryIds |> Random.map Tuple.first |> Random.map (Maybe.withDefault "-1"))
        (TroopCount.random maximumNeutralCountryTroops)


startGame : ConfigurationAttributes -> ( Model, Cmd Msg )
startGame configurationOptions =
    let
        map =
            GameMap.parse Maps.Big.map ActiveGame.pixelsPerMapSquare
    in
    ( GeneratingRandomTroopCounts configurationOptions map
    , Random.generate NeutralCountryTroopCountsGenerated (randomTroopPlacementsGenerator (Dict.keys map.countries))
    )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    case model of
        ConfiguringGame configuringGameSettings ->
            viewGameConfiguration configuringGameSettings

        GeneratingRandomTroopCounts _ _ ->
            Element.layout [] Element.none

        PlayingGame activeGame ->
            viewPlayingGame activeGame



---- Configuration


viewGameConfiguration : ConfigurationAttributes -> Html.Html Msg
viewGameConfiguration gameConfiguration =
    Element.layout [ Element.width Element.fill, Element.centerX ]
        (Element.column
            [ Element.width Element.fill, Element.centerX ]
            [ Element.el
                [ Element.padding 100
                , Element.Font.bold
                , Element.Font.size 80
                , Element.centerX
                , Element.Font.color (Color.darkBlue |> colorToElementColor)
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
                                (Json.map NumberOfPlayersKeyPressed Html.Events.keyCode)
                            )
                        ]
                        { onChange = NumberOfPlayersChanged
                        , text = gameConfiguration.numberOfPlayers
                        , placeholder = Nothing
                        , label =
                            Element.Input.labelLeft
                                [ Element.centerY
                                , Element.paddingEach { top = 0, left = 0, right = 10, bottom = 0 }
                                ]
                                (Element.text "Number of players")
                        }
                    , Element.Input.button
                        (defaultButtonAttributes
                            ++ [ Element.Background.color (Element.rgb255 0 150 0)
                               , Element.width Element.fill
                               , Element.padding 20
                               , Element.centerX
                               , Element.moveDown 30
                               , Element.Font.size 30
                               , Element.Font.color (Color.white |> colorToElementColor)
                               ]
                        )
                        { onPress = Just StartGameClicked, label = centerText "Start Game" }
                    ]
                )
            ]
        )


centerText : String -> Element.Element Msg
centerText text =
    Element.el [ Element.centerX ] (Element.text text)



---- PlayingGame


viewPlayingGame : ActiveGame.ActiveGame -> Html.Html Msg
viewPlayingGame activeGame =
    case activeGame.windowSize of
        Just windowSize ->
            case Element.classifyDevice windowSize |> .class of
                Element.Phone ->
                    viewPlayingGameMobile activeGame

                _ ->
                    viewPlayingGameDesktop activeGame

        Nothing ->
            viewPlayingGameDesktop activeGame


viewPlayingGameMobile : ActiveGame.ActiveGame -> Html.Html Msg
viewPlayingGameMobile activeGame =
    Element.layout [ Element.width Element.fill, Element.Events.onMouseUp MouseUp ]
        (Element.column
            [ Element.centerX, Element.width Element.fill ]
            [ Element.column
                [ Element.centerX
                , Element.width Element.fill
                , Element.alignTop
                ]
                [ Element.column
                    [ Element.width Element.fill
                    , Element.Border.width 1
                    , Element.Border.color black
                    , Element.Border.solid
                    ]
                    ((case activeGame.error of
                        Just error ->
                            [ Element.paragraph [] [ Element.text error ] ]

                        Nothing ->
                            []
                     )
                        ++ [ viewPlayerTurnStatus 10 activeGame.currentPlayerTurn activeGame.players ]
                    )
                , Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (getGameBoardHtml activeGame |> Element.html)
                , viewInfoPanelPhone activeGame
                ]
            ]
        )


viewPlayingGameDesktop : ActiveGame.ActiveGame -> Html.Html Msg
viewPlayingGameDesktop activeGame =
    Element.layout [ Element.width Element.fill, Element.Events.onMouseUp MouseUp ]
        (Element.row
            [ Element.centerX, Element.width Element.fill ]
            [ viewInfoPanelDesktop activeGame
            , Element.column
                [ Element.centerX
                , Element.width Element.fill
                , Element.alignTop
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (getGameBoardHtml activeGame |> Element.html)
                , Element.column
                    [ Element.width Element.fill
                    , Element.Border.width 1
                    , Element.Border.color teal
                    , Element.Border.solid
                    ]
                    ((case activeGame.error of
                        Just error ->
                            [ Element.paragraph [] [ Element.text error ] ]

                        Nothing ->
                            []
                     )
                        ++ [ viewPlayerTurnStatus 20 activeGame.currentPlayerTurn activeGame.players ]
                    )
                ]
            ]
        )


viewInfoPanelPhone : ActiveGame.ActiveGame -> Element.Element Msg
viewInfoPanelPhone activeGame =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 20
        , Element.spacing 20
        , Element.alignTop
        , Element.Border.width 1
        , Element.Border.color black
        , Element.Border.solid
        ]
        [ viewPassButtonIfNecessary activeGame
        , viewPlayerCountryAndTroopCountsMobile activeGame
        , viewConfigureTroopCountIfNecessary activeGame
        , viewCountryInfo activeGame
        , viewShowAvailableMoves activeGame
        ]


viewInfoPanelDesktop : ActiveGame.ActiveGame -> Element.Element Msg
viewInfoPanelDesktop activeGame =
    Element.column
        [ Element.width (Element.px 200)
        , Element.height Element.fill
        , Element.padding 20
        , Element.spacing 20
        , Element.alignTop
        , Element.Border.width 1
        , Element.Border.color black
        , Element.Border.solid
        ]
        [ viewPassButtonIfNecessary activeGame
        , viewPlayerCountryAndTroopCounts activeGame
        , viewConfigureTroopCountIfNecessary activeGame
        , viewCountryInfo activeGame
        , viewShowAvailableMoves activeGame
        ]


viewShowAvailableMoves : ActiveGame.ActiveGame -> Element.Element Msg
viewShowAvailableMoves activeGame =
    Element.row
        [ Element.alignBottom ]
        [ Element.Input.checkbox
            []
            { label =
                Element.Input.labelRight [ Element.Font.size 12 ]
                    (Element.text "Show available moves")
            , icon = Element.Input.defaultCheckbox
            , checked = activeGame.showAvailableMoves
            , onChange = ShowAvailableMovesCheckboxToggled
            }
        ]


viewPassButtonIfNecessary : ActiveGame.ActiveGame -> Element.Element Msg
viewPassButtonIfNecessary activeGame =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 50)
        ]
        (if ActiveGame.canCurrentPlayerPass activeGame then
            Element.Input.button
                (defaultButtonAttributes
                    ++ [ Element.width (Element.px 120)
                       , Element.centerX
                       , 40 |> Element.px |> Element.height
                       , Element.Background.color (Element.rgb255 100 200 100)
                       ]
                )
                { onPress = Just Pass, label = centerText "Pass" }

         else
            Element.none
        )


playerAndTroopCountBorderColor : Element.Color
playerAndTroopCountBorderColor =
    Color.darkGray |> colorToElementColor


viewPlayerCountryAndTroopCounts : ActiveGame.ActiveGame -> Element.Element Msg
viewPlayerCountryAndTroopCounts activeGame =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (ActiveGame.getPlayerCountryAndTroopCounts activeGame
            |> List.map (viewPlayerTroopCount (ActiveGame.getCurrentPlayer activeGame) activeGame.players)
        )


viewPlayerCountryAndTroopCountsMobile : ActiveGame.ActiveGame -> Element.Element Msg
viewPlayerCountryAndTroopCountsMobile activeGame =
    Element.wrappedRow
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (ActiveGame.getPlayerCountryAndTroopCounts activeGame
            |> List.map (viewPlayerTroopCount (ActiveGame.getCurrentPlayer activeGame) activeGame.players)
        )


attackerInfo : ActiveGame.PlayerId -> ActiveGame.ActiveGame -> Dict.Dict Int TroopCount.TroopCount -> Element.Element Msg
attackerInfo countyOwnerPlayerId activeGame attackerStrengthPerPlayer =
    Element.column
        [ Element.width Element.fill, Element.spacing 3 ]
        (attackerStrengthPerPlayer
            |> Dict.toList
            |> List.filter
                (\( playerId, troopCount ) ->
                    ActiveGame.PlayerId playerId /= countyOwnerPlayerId && troopCount /= TroopCount.noTroops
                )
            |> List.map
                (\( playerId, troopCount ) ->
                    case ActiveGame.getPlayer (ActiveGame.PlayerId playerId) activeGame.players of
                        Just player ->
                            Element.row
                                [ Element.width Element.fill
                                , Element.Font.size 14
                                , Element.padding 3
                                , Element.Background.color (player.color |> colorToElementColor)
                                ]
                                [ Element.el [] (Element.text player.name)
                                , Element.el
                                    [ Element.alignRight ]
                                    (troopCount |> TroopCount.toString |> Element.text)
                                ]

                        Nothing ->
                            Element.none
                )
        )


viewCountryInfo : ActiveGame.ActiveGame -> Element.Element Msg
viewCountryInfo activeGame =
    case activeGame.countryBorderHelperOutlines of
        ActiveGame.CountryBorderHelperOutlineActive countryToShowInfoForId ->
            case ActiveGame.findCountryOwner countryToShowInfoForId activeGame.players of
                Just playerId ->
                    case ActiveGame.getPlayer playerId activeGame.players of
                        Just player ->
                            Element.column
                                [ Element.width Element.fill, Element.spacing 5 ]
                                [ Element.el
                                    [ Element.Background.color (player.color |> colorToElementColor)
                                    , Element.Font.size 14
                                    , Element.width Element.fill
                                    , Element.padding 3
                                    ]
                                    (Element.text "Country Information")
                                , Element.row
                                    [ Element.width Element.fill
                                    , Element.Font.size 14
                                    , Element.padding 3
                                    , Element.Border.color (Color.lightGreen |> colorToElementColor)
                                    , Element.Border.solid
                                    , Element.Border.width 3
                                    ]
                                    [ Element.el [] (Element.text "Defense")
                                    , Element.el
                                        [ Element.alignRight ]
                                        (ActiveGame.getCountryDefenseStrength activeGame countryToShowInfoForId |> TroopCount.toString |> Element.text)
                                    ]
                                , Element.column
                                    [ Element.width Element.fill
                                    , Element.Font.size 14
                                    , Element.padding 3
                                    , Element.spacing 3
                                    , Element.Border.color (Color.red |> colorToElementColor)
                                    , Element.Border.solid
                                    , Element.Border.width 3
                                    ]
                                    [ Element.el
                                        [ Element.width Element.fill ]
                                        (Element.text "Opponent attack")
                                    , ActiveGame.getAttackStrengthPerPlayer activeGame countryToShowInfoForId
                                        |> attackerInfo playerId activeGame
                                    ]
                                ]

                        Nothing ->
                            Element.none

                Nothing ->
                    Element.none

        _ ->
            Element.none


viewPlayerTroopCount :
    ActiveGame.PlayerId
    -> Dict.Dict Int ActiveGame.Player
    -> { playerId : ActiveGame.PlayerId, countryCount : Int, troopCount : TroopCount.TroopCount, isAlive : Bool }
    -> Element.Element Msg
viewPlayerTroopCount currentPlayerId players status =
    let
        fontColor =
            if status.isAlive then
                black

            else
                Color.darkGray |> colorToElementColor
    in
    case ActiveGame.getPlayer status.playerId players of
        Just player ->
            Element.column
                ([ Element.spacing 1
                 , Element.width Element.fill
                 , Element.Background.color playerAndTroopCountBorderColor
                 , Element.Font.color fontColor
                 ]
                    ++ playerAndTroopCountBorder currentPlayerId status.playerId
                )
                [ Element.el
                    [ Element.Background.color (player.color |> colorToElementColor)
                    , Element.padding 5
                    , Element.width Element.fill
                    , Element.Font.size 14
                    , Element.Font.bold
                    ]
                    (Element.text <| player.name)
                , Element.column
                    [ Element.Background.color (Color.lightGray |> colorToElementColor)
                    , Element.width Element.fill
                    ]
                    [ Element.row [ Element.spacing 20, Element.Font.size 16 ]
                        [ Element.el
                            [ Element.width (Element.px 80)
                            , Element.alignRight
                            , Element.padding 3
                            ]
                            (Element.el [ Element.alignRight ] (Element.text "Countries"))
                        , Element.el
                            []
                            (Element.text (String.fromInt status.countryCount))
                        ]
                    , Element.row [ Element.spacing 20, Element.Font.size 16 ]
                        [ Element.el
                            [ Element.width (Element.px 80)
                            , Element.padding 3
                            ]
                            (Element.el [ Element.alignRight ] (Element.text "Troops"))
                        , Element.el
                            []
                            (Element.text (TroopCount.toString status.troopCount))
                        ]
                    ]
                ]

        Nothing ->
            Element.none


playerAndTroopCountBorder : ActiveGame.PlayerId -> ActiveGame.PlayerId -> List (Element.Attribute Msg)
playerAndTroopCountBorder currentPlayerId playerIdToDisplay =
    if currentPlayerId == playerIdToDisplay then
        [ Element.Border.solid
        , Element.Border.width 3
        , Element.Border.color black
        ]

    else
        [ Element.Border.solid
        , Element.Border.width 1
        , Element.Border.color playerAndTroopCountBorderColor
        ]


teal : Element.Color
teal =
    Element.rgb255 0 100 100


black : Element.Color
black =
    Element.rgb255 0 0 0


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
    ]


viewConfigureTroopCountIfNecessary : ActiveGame.ActiveGame -> Element.Element Msg
viewConfigureTroopCountIfNecessary activeGame =
    Element.el
        [ Element.width Element.fill
        ]
        (case activeGame |> ActiveGame.troopsToMove of
            Just numberOfTroopsToMove ->
                Element.column
                    [ Element.width Element.fill
                    , Element.padding 10
                    ]
                    [ Element.Input.text
                        (defaultTextInputAttributes ++ [ Element.alignLeft ])
                        { onChange = UpdateNumberOfTroopsToMove
                        , placeholder = Nothing
                        , label = Element.Input.labelAbove (defaultLabelAttributes ++ [ Element.alignLeft ]) (Element.text "Number of troops to move")
                        , text = numberOfTroopsToMove
                        }
                    , Element.Input.button
                        (defaultButtonAttributes
                            ++ [ Element.width Element.fill
                               , Element.centerX
                               , Element.Font.color (Element.rgb255 255 255 255)
                               , Element.Background.color (Element.rgb255 255 63 63)
                               , Element.moveDown 10
                               ]
                        )
                        { onPress = Just CancelMovingTroops, label = centerText "Cancel" }
                    ]

            Nothing ->
                Element.none
        )


viewPlayerTurnStatus : Int -> ActiveGame.PlayerTurn -> Dict.Dict Int ActiveGame.Player -> Element.Element Msg
viewPlayerTurnStatus fontSize playerTurn players =
    Element.el
        [ Element.width Element.fill
        , Element.Background.color (ActiveGame.getPlayerColorFromPlayerTurn players playerTurn |> colorToElementColor)
        , Element.padding 5
        ]
        (Element.el
            [ Element.width Element.fill ]
            (Element.paragraph [ Element.Font.size fontSize ]
                [ Element.text
                    (ActiveGame.playerTurnToString players playerTurn)
                ]
            )
        )


colorToElementColor : Color.Color -> Element.Color
colorToElementColor color =
    color |> Color.toRgba |> Element.fromRgb


getWaterCollage : GameMap.GameMap -> Collage.Collage Msg
getWaterCollage gameMap =
    let
        background =
            Collage.polygon
                [ ( 0, 0 )
                , ( 0, gameMap.dimensions |> Tuple.second )
                , ( gameMap.dimensions |> Tuple.first, gameMap.dimensions |> Tuple.second )
                , ( gameMap.dimensions |> Tuple.first, 0 )
                ]

        backgroundWater =
            background
                |> Collage.filled (Collage.uniform Color.blue)

        backgroundBorder =
            background
                |> Collage.outlined (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 8.0) (Collage.uniform Color.black))
    in
    Collage.group [ backgroundBorder, backgroundWater ]


getGameBoardHtml : ActiveGame.ActiveGame -> Html.Html Msg
getGameBoardHtml activeGame =
    case ActiveGame.getCountriesToRender activeGame of
        Just countriesToRender ->
            let
                waterCollage : Collage.Collage Msg
                waterCollage =
                    getWaterCollage activeGame.map

                countriesCollage =
                    countriesToRender
                        |> List.map getCountryCollage
                        |> Collage.group

                troopCountFontSize =
                    case activeGame.windowSize of
                        Just windowSize ->
                            case Element.classifyDevice windowSize |> .class of
                                Element.Phone ->
                                    200

                                _ ->
                                    100

                        Nothing ->
                            100

                troopCountsCollage =
                    countriesToRender
                        |> List.map (getTroopCountCollage troopCountFontSize)
                        |> Collage.group

                gameBoardHeight =
                    Collage.Layout.height waterCollage

                gameBoardWidth =
                    Collage.Layout.width waterCollage

                countryEventHandlers =
                    countriesToRender
                        |> List.map getEventHandlersForCountry
                        |> Collage.group

                capitolDots =
                    countriesToRender
                        |> List.map renderCapitolDots
                        |> Collage.group

                countryHighlights =
                    if activeGame.showAvailableMoves then
                        countriesToRender
                            |> List.map countryHighlight
                            |> Collage.group

                    else
                        Collage.group []

                portCollages =
                    countriesToRender
                        |> List.map getPortCollage
                        |> Collage.group

                countryInfoHighlights =
                    countriesToRender
                        |> List.map (getCountryInfoPolygonBorder activeGame)
                        |> Collage.group
            in
            Collage.group
                [ countryEventHandlers
                , countryInfoHighlights
                , countryHighlights
                , troopCountsCollage
                , capitolDots
                , portCollages
                , countriesCollage
                , waterCollage
                ]
                |> Collage.Render.svgExplicit
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "max-height" "100%"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "left" "0"
                    , Html.Attributes.attribute "width" "0"
                    , Html.Attributes.attribute
                        "viewBox"
                        ((0 * gameBoardWidth |> String.fromFloat)
                            ++ " "
                            ++ (-1 * gameBoardHeight |> String.fromFloat)
                            ++ " "
                            ++ (1 * gameBoardWidth |> String.fromFloat)
                            ++ " "
                            ++ (1 * gameBoardHeight |> String.fromFloat)
                        )
                    ]

        Nothing ->
            Html.div [] [ Html.text "Kaboom" ]


countryHighlight : ActiveGame.CountryToRender -> Collage.Collage Msg
countryHighlight countryToRender =
    let
        maybeCountryCanBeClickedHighlight =
            if countryToRender.canBeClicked then
                Just (countryHighlightCollage 0.99 countryToRender)

            else if countryToRender.isBeingMovedFrom then
                Just ([ getGrayedOutCountryCollage countryToRender, countryHighlightCollage 1.0 countryToRender ] |> Collage.group)

            else
                Just (getGrayedOutCountryCollage countryToRender)
    in
    [ maybeCountryCanBeClickedHighlight ]
        |> List.foldl
            (\maybeHighlight result ->
                maybeHighlight |> Maybe.map (\highlight -> highlight :: result) |> Maybe.withDefault result
            )
            []
        |> Collage.group


getEventHandlersForCountry : ActiveGame.CountryToRender -> Collage.Collage Msg
getEventHandlersForCountry countryToRender =
    countryToRender.polygonPoints
        |> Collage.polygon
        |> Collage.filled (Color.rgba 0 0 0 0 |> Collage.uniform)
        |> Collage.Events.onMouseUp (\_ -> CountryMouseUp countryToRender.id)
        |> Collage.Events.onMouseDown (\_ -> CountryMouseDown countryToRender.id)
        |> Collage.Events.onMouseLeave (\_ -> CountryMouseOut countryToRender.id)


getPortCollage : ActiveGame.CountryToRender -> Collage.Collage Msg
getPortCollage countryToRender =
    (case countryToRender.portSegments of
        Just portSegments ->
            [ renderPort portSegments ]

        Nothing ->
            []
    )
        |> Collage.group


countryHighlightCollage : Float -> ActiveGame.CountryToRender -> Collage.Collage Msg
countryHighlightCollage scale countryToRender =
    let
        ( centerX, centerY ) =
            countryToRender.center

        updatedPoints =
            countryToRender.polygonPoints
                |> List.map
                    (\( x, y ) ->
                        ( (x - centerX) * scale + centerX, (y - centerY) * scale + centerY )
                    )
    in
    updatedPoints
        |> Collage.polygon
        |> Collage.outlined
            (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 6.0)
                (Collage.uniform countryCanBeClickedColor)
            )


countryCanBeClickedColor : Color.Color
countryCanBeClickedColor =
    Color.white


getGrayedOutCountryCollage : ActiveGame.CountryToRender -> Collage.Collage Msg
getGrayedOutCountryCollage countryToRender =
    countryToRender.polygonPoints
        |> Collage.polygon
        |> Collage.filled (Color.rgba 0 0 0 0.5 |> Collage.uniform)


getTroopCountCollage : Int -> ActiveGame.CountryToRender -> Collage.Collage Msg
getTroopCountCollage fontSize countryToRender =
    if TroopCount.hasTroops countryToRender.troopCount then
        countryToRender.troopCount
            |> TroopCount.toString
            |> Collage.Text.fromString
            |> Collage.Text.color Color.black
            |> Collage.Text.size fontSize
            |> Collage.rendered
            |> Collage.shift countryToRender.center

    else
        Collage.group []


getCountryCollage : ActiveGame.CountryToRender -> Collage.Collage Msg
getCountryCollage countryToRender =
    let
        countryPolygon =
            Collage.polygon countryToRender.polygonPoints

        fill =
            countryPolygon
                |> Collage.filled (Collage.uniform countryToRender.color)

        border =
            countryPolygon
                |> Collage.outlined
                    (Collage.solid 30.0
                        (Collage.uniform countryBorderColor)
                    )
    in
    Collage.group [ fill, border ]


renderCapitolDots : ActiveGame.CountryToRender -> Collage.Collage Msg
renderCapitolDots countryToRender =
    let
        ( capitolDot, capitolDotsCoords ) =
            case countryToRender.capitolDots of
                Just capitolDots ->
                    ( [ Collage.square (toFloat ActiveGame.pixelsPerMapSquare / 10.0)
                            |> Collage.filled (Collage.uniform Color.black)
                      ]
                    , capitolDots
                    )

                Nothing ->
                    ( [], Set.empty )

        renderedDot =
            capitolDot
                |> Collage.group
    in
    capitolDotsCoords
        |> Set.foldl
            (\coordinates result ->
                (renderedDot |> Collage.shift coordinates) :: result
            )
            []
        |> Collage.group


getCountryInfoPolygonBorder : ActiveGame.ActiveGame -> ActiveGame.CountryToRender -> Collage.Collage Msg
getCountryInfoPolygonBorder activeGame countryToRender =
    case getCountryInfoStatus activeGame countryToRender.id of
        CountryInfoSelectedCountry ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 6.0)
                        (Collage.uniform Color.white)
                    )

        CountryInfoDefending ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 6.0)
                        (Collage.uniform Color.green)
                    )

        CountryInfoAttacking ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 6.0)
                        (Collage.uniform Color.red)
                    )

        NoInfo ->
            Collage.group []


type CountryInfoStatus
    = CountryInfoSelectedCountry
    | CountryInfoDefending
    | CountryInfoAttacking
    | NoInfo


getCountryInfoStatus : ActiveGame.ActiveGame -> GameMap.CountryId -> CountryInfoStatus
getCountryInfoStatus activeGame countryId =
    case activeGame.countryBorderHelperOutlines of
        ActiveGame.CountryBorderHelperOutlineActive countryToShowInfoForId ->
            if countryToShowInfoForId == countryId then
                CountryInfoSelectedCountry

            else if ActiveGame.isCountryDefending activeGame countryToShowInfoForId countryId then
                CountryInfoDefending

            else if ActiveGame.isCountryAttacking activeGame countryToShowInfoForId countryId then
                CountryInfoAttacking

            else
                NoInfo

        _ ->
            NoInfo


renderPort : Set.Set GameMap.BorderSegment -> Collage.Collage msg
renderPort waterEdges =
    waterEdges
        |> Set.toList
        |> List.map
            (\( point1, point2 ) ->
                Collage.segment point1 point2
                    |> Collage.traced
                        (Collage.broken [ ( 3, 10 ) ]
                            ((ActiveGame.pixelsPerMapSquare |> toFloat) / 2.0)
                            (Collage.uniform Color.gray)
                        )
            )
        |> Collage.group



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        PlayingGame activeGame ->
            let
                subscription =
                    if ActiveGame.waitingToShowCountryHelperOutlines activeGame then
                        Time.every countryOutlineDelayMilliseconds (always ShowCountryBorderHelper)

                    else
                        Sub.none
            in
            Sub.batch [ subscription, Browser.Events.onResize (\x y -> WindowResized x y) ]

        _ ->
            Sub.none
