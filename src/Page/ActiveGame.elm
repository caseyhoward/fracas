module Page.ActiveGame exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import ActiveGame
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
import Session
import Set
import Time
import TroopCount
import ViewHelpers


type alias Model =
    { activeGame : ActiveGame.ActiveGame
    , session : Session.Session
    }



-- { currentPlayerTurn : ActiveGame.PlayerTurn
-- , map : GameMap.GameMap
-- , players : Dict.Dict Int ActiveGame.Player
-- , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
-- , error : Maybe String
-- , numberOfPlayers : Int
-- , countryBorderHelperOutlines : ActiveGame.CountryBorderHelperOutlineStatus
-- , showAvailableMoves : Bool
-- }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    case Session.gameSettings session of
        Just { numberOfPlayers, gameMap, neutralCountryTroopCounts } ->
            ( { activeGame = ActiveGame.start gameMap numberOfPlayers neutralCountryTroopCounts, session = session }
            , Cmd.none
            )

        Nothing ->
            Debug.todo ""



-- ( { model | error = "Game not configured" }, Cmd.none )
---- UPDATE ----


type Msg
    = CountryMouseUp GameMap.CountryId
    | CountryMouseDown GameMap.CountryId
    | CountryMouseOut GameMap.CountryId
    | MouseUp
    | Pass
    | UpdateNumberOfTroopsToMove String
    | CancelMovingTroops
    | ShowCountryBorderHelper
    | ShowAvailableMovesCheckboxToggled Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        activeGame =
            model.activeGame
    in
    case msg of
        CountryMouseUp clickedCountryId ->
            ( { model | activeGame = ActiveGame.handleCountryMouseUpFromPlayer clickedCountryId activeGame }
            , Cmd.none
            )

        CountryMouseDown clickedCountryId ->
            ( { model | activeGame = ActiveGame.handleCountryMouseDown clickedCountryId activeGame }, Cmd.none )

        CountryMouseOut mouseOutCountryId ->
            ( { model | activeGame = ActiveGame.handleCountryMouseOut mouseOutCountryId activeGame }, Cmd.none )

        ShowAvailableMovesCheckboxToggled isChecked ->
            ( { model | activeGame = { activeGame | showAvailableMoves = isChecked } }, Cmd.none )

        MouseUp ->
            ( { model | activeGame = ActiveGame.stopShowingCountryHelperOutlines activeGame }, Cmd.none )

        Pass ->
            ( { model | activeGame = ActiveGame.pass activeGame }, Cmd.none )

        UpdateNumberOfTroopsToMove numberOfTroopsToMoveString ->
            ( { model | activeGame = ActiveGame.updateNumberOfTroopsToMove numberOfTroopsToMoveString activeGame }, Cmd.none )

        CancelMovingTroops ->
            ( { model | activeGame = ActiveGame.cancelMovingTroops activeGame }, Cmd.none )

        ShowCountryBorderHelper ->
            ( { model | activeGame = ActiveGame.makeCountryHelperOutlinesActive activeGame }, Cmd.none )



---- VIEW ----


view : Model -> { title : String, content : Html.Html Msg }
view model =
    { content =
        case model.session.windowSize of
            Just windowSize ->
                let
                    device =
                        Element.classifyDevice windowSize
                in
                case Element.classifyDevice windowSize |> .class of
                    Element.Phone ->
                        viewPlayingGameMobile model.activeGame device

                    _ ->
                        viewPlayingGameDesktop model.activeGame device

            Nothing ->
                viewPlayingGameDesktop model.activeGame (Element.classifyDevice { width = 1920, height = 1080 })
    , title = "Fracas"
    }


countryBorderColor : Color.Color
countryBorderColor =
    Color.rgb255 100 100 100


viewPlayingGameMobile : ActiveGame.ActiveGame -> Element.Device -> Html.Html Msg
viewPlayingGameMobile activeGame device =
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
                        ++ [ viewPlayerTurnStatus 38 10 activeGame.currentPlayerTurn activeGame.players ]
                    )
                , Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (getGameBoardHtml activeGame device |> Element.html)
                , viewInfoPanelPhone activeGame
                ]
            ]
        )


viewPlayingGameDesktop : ActiveGame.ActiveGame -> Element.Device -> Html.Html Msg
viewPlayingGameDesktop activeGame device =
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
                    (getGameBoardHtml activeGame device |> Element.html)
                , Element.column
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
                        ++ [ viewPlayerTurnStatus 55 20 activeGame.currentPlayerTurn activeGame.players ]
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
        [ viewPassButtonIfNecessary activeGame.currentPlayerTurn
        , viewPlayerCountryAndTroopCountsMobile activeGame.currentPlayerTurn activeGame.players
        , viewConfigureTroopCountIfNecessary activeGame.currentPlayerTurn
        , viewCountryInfo activeGame
        , viewShowAvailableMoves activeGame.showAvailableMoves
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
        [ viewPassButtonIfNecessary activeGame.currentPlayerTurn
        , viewPlayerCountryAndTroopCounts activeGame.currentPlayerTurn activeGame.players
        , viewConfigureTroopCountIfNecessary activeGame.currentPlayerTurn
        , viewCountryInfo activeGame
        , viewShowAvailableMoves activeGame.showAvailableMoves
        ]


viewShowAvailableMoves : Bool -> Element.Element Msg
viewShowAvailableMoves showAvailableMoves =
    Element.row
        [ Element.alignBottom ]
        [ Element.Input.checkbox
            []
            { label =
                Element.Input.labelRight [ Element.Font.size 12 ]
                    (Element.text "Show available moves")
            , icon = Element.Input.defaultCheckbox
            , checked = showAvailableMoves
            , onChange = ShowAvailableMovesCheckboxToggled
            }
        ]


viewPassButtonIfNecessary : ActiveGame.PlayerTurn -> Element.Element Msg
viewPassButtonIfNecessary currentPlayerTurn =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 50)
        ]
        (if ActiveGame.canCurrentPlayerPass currentPlayerTurn then
            Element.Input.button
                (ViewHelpers.defaultButtonAttributes
                    ++ [ Element.width (Element.px 120)
                       , Element.centerX
                       , 40 |> Element.px |> Element.height
                       , Element.Background.color (Element.rgb255 100 200 100)
                       ]
                )
                { onPress = Just Pass, label = ViewHelpers.centerText "Pass" }

         else
            Element.none
        )


playerAndTroopCountBorderColor : Element.Color
playerAndTroopCountBorderColor =
    Color.darkGray |> ViewHelpers.colorToElementColor


viewPlayerCountryAndTroopCounts : ActiveGame.PlayerTurn -> Dict.Dict Int ActiveGame.Player -> Element.Element Msg
viewPlayerCountryAndTroopCounts currentPlayerTurn players =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (ActiveGame.getPlayerCountryAndTroopCounts { currentPlayerTurn = currentPlayerTurn, players = players }
            |> List.map (viewPlayerTroopCount (ActiveGame.getCurrentPlayer currentPlayerTurn) players)
        )


viewPlayerCountryAndTroopCountsMobile : ActiveGame.PlayerTurn -> Dict.Dict Int ActiveGame.Player -> Element.Element Msg
viewPlayerCountryAndTroopCountsMobile currentPlayerTurn players =
    Element.wrappedRow
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (ActiveGame.getPlayerCountryAndTroopCounts { currentPlayerTurn = currentPlayerTurn, players = players }
            |> List.map (viewPlayerTroopCount (ActiveGame.getCurrentPlayer currentPlayerTurn) players)
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
                                , Element.Background.color (player.color |> ViewHelpers.colorToElementColor)
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
                                    [ Element.Background.color (player.color |> ViewHelpers.colorToElementColor)
                                    , Element.Font.size 14
                                    , Element.width Element.fill
                                    , Element.padding 3
                                    ]
                                    (Element.text "Country Information")
                                , Element.row
                                    [ Element.width Element.fill
                                    , Element.Font.size 14
                                    , Element.padding 3
                                    , Element.Border.color (Color.lightGreen |> ViewHelpers.colorToElementColor)
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
                                    , Element.Border.color (Color.red |> ViewHelpers.colorToElementColor)
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
                Color.darkGray |> ViewHelpers.colorToElementColor
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
                    [ Element.Background.color (player.color |> ViewHelpers.colorToElementColor)
                    , Element.padding 5
                    , Element.width Element.fill
                    , Element.Font.size 14
                    , Element.Font.bold
                    ]
                    (Element.text <| player.name)
                , Element.column
                    [ Element.Background.color (Color.lightGray |> ViewHelpers.colorToElementColor)
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


viewConfigureTroopCountIfNecessary : ActiveGame.PlayerTurn -> Element.Element Msg
viewConfigureTroopCountIfNecessary currentPlayerTurn =
    Element.el
        [ Element.width Element.fill
        ]
        (case currentPlayerTurn |> ActiveGame.troopsToMove of
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
                        (ViewHelpers.defaultButtonAttributes
                            ++ [ Element.width Element.fill
                               , Element.centerX
                               , Element.Font.color (Element.rgb255 255 255 255)
                               , Element.Background.color (Element.rgb255 255 63 63)
                               , Element.moveDown 10
                               ]
                        )
                        { onPress = Just CancelMovingTroops, label = ViewHelpers.centerText "Cancel" }
                    ]

            Nothing ->
                Element.none
        )


viewPlayerTurnStatus : Int -> Int -> ActiveGame.PlayerTurn -> Dict.Dict Int ActiveGame.Player -> Element.Element Msg
viewPlayerTurnStatus height fontSize playerTurn players =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px height)
        , Element.Background.color (ActiveGame.getPlayerColorFromPlayerTurn players playerTurn |> ViewHelpers.colorToElementColor)
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
                |> Collage.outlined (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 8.0) (Collage.uniform Color.black))
    in
    Collage.group [ backgroundBorder, backgroundWater ]


getGameBoardHtml : ActiveGame.ActiveGame -> Element.Device -> Html.Html Msg
getGameBoardHtml activeGame device =
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
                    case device.class of
                        Element.Phone ->
                            200

                        _ ->
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
            (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 6.0)
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
                    ( [ Collage.square (toFloat ViewHelpers.pixelsPerMapSquare / 10.0)
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
                    (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 6.0)
                        (Collage.uniform Color.white)
                    )

        CountryInfoDefending ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 6.0)
                        (Collage.uniform Color.green)
                    )

        CountryInfoAttacking ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 6.0)
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
                            ((ViewHelpers.pixelsPerMapSquare |> toFloat) / 2.0)
                            (Collage.uniform Color.gray)
                        )
            )
        |> Collage.group



-- Subscriptions


countryOutlineDelayMilliseconds : Float
countryOutlineDelayMilliseconds =
    300


subscriptions : Model -> Sub Msg
subscriptions model =
    if ActiveGame.waitingToShowCountryHelperOutlines model.activeGame then
        Time.every countryOutlineDelayMilliseconds (always ShowCountryBorderHelper)

    else
        Sub.none


toSession : Model -> Session.Session
toSession model =
    model.session
