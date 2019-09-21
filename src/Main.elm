module Main exposing (main)

-- import Maps.Big

import ActiveGame
import Browser
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
import Maps.MobileFriendlier
import Maps.UnitedStates
import Random
import Random.Dict
import Random.List
import Set
import Time
import TroopCount



-- Settings


maximumNeutralCountryTroops : Int
maximumNeutralCountryTroops =
    10


countryBorderColor : Color.Color
countryBorderColor =
    Color.black


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
                CountryMouseUp clickedCountryId ->
                    ( PlayingGame (ActiveGame.handleCountryMouseUpFromPlayer clickedCountryId attributes)
                    , Cmd.none
                    )

                CountryMouseDown clickedCountryId ->
                    ( PlayingGame (ActiveGame.handleCountryMouseDown clickedCountryId attributes), Cmd.none )

                CountryMouseOut mouseOutCountryId ->
                    ( PlayingGame (ActiveGame.handleCountryMouseOut mouseOutCountryId attributes), Cmd.none )

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
            GameMap.parse Maps.UnitedStates.map ActiveGame.pixelsPerMapSquare
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
    Element.layout [ Element.width Element.fill, Element.Events.onMouseUp MouseUp ]
        (Element.row
            [ Element.centerX, Element.width Element.fill ]
            [ viewInfoPanel activeGame
            , Element.column
                [ Element.centerX
                , Element.width Element.fill
                , Element.alignTop
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (renderGameBoard activeGame |> Element.html)
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
                        ++ [ viewPlayerTurnStatus activeGame.currentPlayerTurn activeGame.players ]
                    )
                ]
            ]
        )


viewInfoPanel : ActiveGame.ActiveGame -> Element.Element Msg
viewInfoPanel activeGame =
    Element.column
        [ Element.width (Element.px 200)
        , Element.height Element.fill
        , Element.padding 20
        , Element.spacing 20
        , Element.alignTop
        , Element.Border.width 1
        , Element.Border.color teal
        , Element.Border.solid
        ]
        [ viewPassButtonIfNecessary activeGame
        , viewPlayerCountryAndTroopCounts activeGame
        , viewConfigureTroopCountIfNecessary activeGame
        , viewCountryInfo activeGame
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


viewCountryInfo : ActiveGame.ActiveGame -> Element.Element Msg
viewCountryInfo activeGame =
    case activeGame.countryBorderHelperOutlines of
        ActiveGame.CountryBorderHelperOutlineActive countryToShowInfoForId ->
            case ActiveGame.findCountryOwner countryToShowInfoForId activeGame.players of
                Just playerId ->
                    case ActiveGame.getPlayer playerId activeGame.players of
                        Just player ->
                            Element.column
                                [ Element.width Element.fill ]
                                [ Element.el
                                    [ Element.Background.color (player.color |> colorToElementColor)
                                    , Element.Font.size 14
                                    , Element.width Element.fill
                                    , Element.padding 3
                                    ]
                                    (Element.text "Country Information")
                                , Element.row [ Element.width Element.fill, Element.Font.size 14, Element.padding 3 ]
                                    [ Element.el [] (Element.text "Defense")
                                    , Element.el [ Element.alignRight ] (ActiveGame.getCountryDefenseStrength activeGame countryToShowInfoForId |> TroopCount.toString |> Element.text)
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

    -- , Element.moveUp 7
    -- , Element.moveRight 5
    -- , Element.paddingEach { left = 5, top = 0, right = 5, bottom = 0 }
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


viewPlayerTurnStatus : ActiveGame.PlayerTurn -> Dict.Dict Int ActiveGame.Player -> Element.Element Msg
viewPlayerTurnStatus playerTurn players =
    Element.el
        [ Element.width Element.fill
        , Element.Background.color (ActiveGame.getPlayerColorFromPlayerTurn players playerTurn |> colorToElementColor)
        , Element.padding 5
        ]
        (Element.el
            [ Element.width Element.fill ]
            (Element.paragraph []
                [ Element.text
                    (ActiveGame.playerTurnToString players playerTurn)
                ]
            )
        )


colorToElementColor : Color.Color -> Element.Color
colorToElementColor color =
    color |> Color.toRgba |> Element.fromRgb



-- Rendering


renderGameBoard : ActiveGame.ActiveGame -> Html.Html Msg
renderGameBoard activeGame =
    let
        countryCollages : List (Collage.Collage Msg)
        countryCollages =
            activeGame.map.countries
                |> GameMap.getCountryIds
                |> List.sortBy
                    (\countryId ->
                        case activeGame.countryBorderHelperOutlines of
                            ActiveGame.CountryBorderHelperOutlineActive countryToShowInfoForId ->
                                if countryToShowInfoForId == countryId then
                                    0

                                else if ActiveGame.isCountryDefending activeGame countryToShowInfoForId countryId then
                                    1

                                else if ActiveGame.isCountryAttacking activeGame countryToShowInfoForId countryId then
                                    1

                                else
                                    100

                            _ ->
                                100
                    )
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
                |> Collage.outlined (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 8.0) (Collage.uniform Color.black))

        gameBoardCollage =
            Collage.group (countryCollages ++ [ backgroundBorder, backgroundWater ])

        gameBoardHeight =
            Collage.Layout.height gameBoardCollage

        gameBoardWidth =
            Collage.Layout.width gameBoardCollage
    in
    gameBoardCollage
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


renderCountry : GameMap.CountryId -> ActiveGame.ActiveGame -> Collage.Collage Msg
renderCountry countryId activeGame =
    case ( ActiveGame.findCountryOwner countryId activeGame.players, GameMap.getCountry countryId activeGame.map.countries ) of
        ( Just countryOwnerId, Just country ) ->
            case
                ( ActiveGame.getPlayer countryOwnerId activeGame.players
                , ActiveGame.getTroopCountForCountry countryId activeGame.players
                , ActiveGame.getCountryHasPort countryId activeGame.players
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
                               , renderArea country.polygon player.color player.capitolStatus (getCountryInfoStatus activeGame countryId) countryId
                               ]
                        )
                        |> Collage.Events.onMouseUp (\_ -> CountryMouseUp countryId)
                        |> Collage.Events.onMouseDown (\_ -> CountryMouseDown countryId)
                        |> Collage.Events.onMouseLeave (\_ -> CountryMouseOut countryId)

                _ ->
                    Collage.Text.fromString "Error rendering country 1" |> Collage.rendered

        ( Nothing, Just country ) ->
            case ActiveGame.getTroopCount countryId activeGame.neutralCountryTroops of
                Just troopCount ->
                    Collage.group
                        [ renderTroopCount country.center troopCount
                        , renderArea country.polygon Color.gray ActiveGame.NoCapitol (getCountryInfoStatus activeGame countryId) countryId
                        ]
                        |> Collage.Events.onMouseUp (\_ -> CountryMouseUp countryId)
                        |> Collage.Events.onMouseDown (\_ -> CountryMouseDown countryId)
                        |> Collage.Events.onMouseLeave (\_ -> CountryMouseOut countryId)

                _ ->
                    Collage.group
                        [ renderArea country.polygon Color.gray ActiveGame.NoCapitol (getCountryInfoStatus activeGame countryId) countryId
                        ]
                        |> Collage.Events.onMouseUp (\_ -> CountryMouseUp countryId)
                        |> Collage.Events.onMouseDown (\_ -> CountryMouseDown countryId)
                        |> Collage.Events.onMouseLeave (\_ -> CountryMouseOut countryId)

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
                            ((ActiveGame.pixelsPerMapSquare |> toFloat) / 2.0)
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
            |> Collage.Text.size (toFloat ActiveGame.pixelsPerMapSquare * 0.8 |> ceiling)
            |> Collage.rendered
            |> Collage.shift ( (toFloat medianX + 0.5) * toFloat ActiveGame.pixelsPerMapSquare, (toFloat medianY + 0.5) * toFloat ActiveGame.pixelsPerMapSquare )

    else
        Collage.group []


renderArea : List ( Float, Float ) -> Color.Color -> ActiveGame.CapitolStatus -> CountryInfoStatus -> GameMap.CountryId -> Collage.Collage msg
renderArea polygonPoints color capitolStatus countryInfoStatus countryId =
    let
        ( capitolDot, capitolDotsCoords ) =
            case capitolStatus of
                ActiveGame.Capitol capitolId coords ->
                    if countryId == capitolId then
                        ( [ Collage.square (toFloat ActiveGame.pixelsPerMapSquare / 10.0)
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
            case countryInfoStatus of
                CountryInfoSelectedCountry ->
                    polygon
                        |> Collage.outlined
                            (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 6.0)
                                (Collage.uniform Color.white)
                            )

                CountryInfoDefending ->
                    polygon
                        |> Collage.outlined
                            (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 6.0)
                                (Collage.uniform Color.green)
                            )

                CountryInfoAttacking ->
                    polygon
                        |> Collage.outlined
                            (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 6.0)
                                (Collage.uniform Color.red)
                            )

                NoInfo ->
                    polygon
                        |> Collage.outlined
                            (Collage.solid (toFloat ActiveGame.pixelsPerMapSquare / 24.0)
                                (Collage.uniform countryBorderColor)
                            )

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
subscriptions model =
    case model of
        PlayingGame activeGame ->
            if ActiveGame.waitingToShowCountryHelperOutlines activeGame then
                Time.every countryOutlineDelayMilliseconds (always ShowCountryBorderHelper)

            else
                Sub.none

        _ ->
            Sub.none
