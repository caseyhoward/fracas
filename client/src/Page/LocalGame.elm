module Page.LocalGame exposing
    ( CountryBorderHelperOutlineStatus(..)
    , GameLoadedModel
    , Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    , viewGameWrapMessage
    )

import Browser.Events
import Collage
import Collage.Events
import Collage.Layout
import Collage.Render
import Collage.Text
import Colors
import Country
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Graphql.Http
import Html
import Html.Attributes
import LocalGame
import Map
import Player
import PlayerTurn
import RemoteData
import Session
import Set
import Time
import TroopCount
import ViewHelpers


type Model
    = GameLoading GameLoadingModel
    | GameLoaded GameLoadedModel
    | GameSaving GameLoadedModel (RemoteData.RemoteData (Graphql.Http.Error LocalGame.Game) LocalGame.Game)


type alias GameLoadingModel =
    { activeGame : RemoteData.RemoteData (Graphql.Http.Error LocalGame.Game) LocalGame.Game
    , error : Maybe String
    , session : Session.Session
    , playerId : Player.Id
    }


type alias GameLoadedModel =
    { activeGame : LocalGame.Game
    , showAvailableMoves : Bool
    , session : Session.Session
    , error : Maybe String
    , playerId : Player.Id
    , countryBorderHelperOutlineStatus : CountryBorderHelperOutlineStatus
    }


type CountryBorderHelperOutlineStatus
    = CountryBorderHelperOutlineWaitingForDelay Country.Id
    | CountryBorderHelperOutlineInactive
    | CountryBorderHelperOutlineActive Country.Id


init : Session.Session -> LocalGame.Id -> Player.Id -> ( Model, Cmd Msg )
init session activeGameId playerId =
    ( GameLoading
        { activeGame = RemoteData.NotAsked
        , error = Nothing
        , playerId = playerId
        , session = session
        }
    , LocalGame.get session.apiUrl activeGameId GotGame
    )


toSession : Model -> Session.Session
toSession model =
    case model of
        GameSaving gameLoadedModel _ ->
            gameLoadedModel.session

        GameLoaded gameLoadedModel ->
            gameLoadedModel.session

        GameLoading gameLoadingModel ->
            gameLoadingModel.session



---- UPDATE ----


type Msg
    = CountryMouseUp Country.Id
    | CountryMouseDown Country.Id
    | CountryMouseOut Country.Id
    | GotGame (RemoteData.RemoteData (Graphql.Http.Error LocalGame.Game) LocalGame.Game)
    | MouseUp
    | Pass
    | UpdateNumberOfTroopsToMove String
    | CancelMovingTroops
    | ShowCountryBorderHelper
    | ShowAvailableMovesCheckboxToggled Bool
    | WindowResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameLoading gameLoadingModel ->
            case msg of
                GotGame gameRemoteData ->
                    case gameRemoteData of
                        RemoteData.Success game ->
                            ( GameLoaded
                                { activeGame = game
                                , showAvailableMoves = False
                                , session = gameLoadingModel.session
                                , error = Nothing
                                , playerId = gameLoadingModel.playerId
                                , countryBorderHelperOutlineStatus = CountryBorderHelperOutlineInactive
                                }
                            , Cmd.none
                            )

                        RemoteData.Failure error ->
                            ( GameLoading { gameLoadingModel | error = Just (ViewHelpers.errorToString error) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                WindowResized width height ->
                    ( GameLoading { gameLoadingModel | session = gameLoadingModel.session |> Session.updateWindowSize { width = width, height = height } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameSaving gameLoadedModel savingGame ->
            case msg of
                GotGame gameRemoteData ->
                    case gameRemoteData of
                        RemoteData.Success game ->
                            ( GameLoaded { gameLoadedModel | activeGame = game }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                WindowResized width height ->
                    ( GameSaving { gameLoadedModel | session = gameLoadedModel.session |> Session.updateWindowSize { width = width, height = height } } savingGame, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GameLoaded gameLoadedModel ->
            case msg of
                CountryMouseUp clickedCountryId ->
                    handleCountryMouseUpFromPlayer gameLoadedModel.session.apiUrl clickedCountryId gameLoadedModel

                CountryMouseDown clickedCountryId ->
                    ( handleCountryMouseDown clickedCountryId gameLoadedModel |> GameLoaded, Cmd.none )

                CountryMouseOut mouseOutCountryId ->
                    ( handleCountryMouseOut mouseOutCountryId gameLoadedModel |> GameLoaded, Cmd.none )

                ShowAvailableMovesCheckboxToggled isChecked ->
                    ( GameLoaded { gameLoadedModel | showAvailableMoves = isChecked }, Cmd.none )

                MouseUp ->
                    ( stopShowingCountryHelperOutlines gameLoadedModel |> GameLoaded, Cmd.none )

                Pass ->
                    ( updateModelWithGameResult (LocalGame.pass gameLoadedModel.activeGame) gameLoadedModel |> GameLoaded, Cmd.none )

                UpdateNumberOfTroopsToMove numberOfTroopsToMoveString ->
                    ( GameLoaded { gameLoadedModel | activeGame = LocalGame.updateNumberOfTroopsToMove numberOfTroopsToMoveString gameLoadedModel.activeGame }, Cmd.none )

                CancelMovingTroops ->
                    ( GameLoaded { gameLoadedModel | activeGame = LocalGame.cancelMovingTroops gameLoadedModel.activeGame }, Cmd.none )

                ShowCountryBorderHelper ->
                    ( makeCountryHelperOutlinesActive gameLoadedModel |> GameLoaded, Cmd.none )

                WindowResized width height ->
                    ( GameLoaded { gameLoadedModel | session = gameLoadedModel.session |> Session.updateWindowSize { width = width, height = height } }, Cmd.none )

                GotGame _ ->
                    ( model, Cmd.none )


updateModelWithGameResult : Result LocalGame.Error LocalGame.Game -> GameLoadedModel -> GameLoadedModel
updateModelWithGameResult result model =
    case result of
        Ok activeGame ->
            { model | activeGame = activeGame, error = Nothing }

        Err error ->
            { model | error = Just (LocalGame.errorToString error) }


handleCountryMouseUpFromPlayer : String -> Country.Id -> GameLoadedModel -> ( Model, Cmd Msg )
handleCountryMouseUpFromPlayer apiUrl clickedCountryId gameLoadedModel =
    case gameLoadedModel.countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineActive _ ->
            ( GameLoaded gameLoadedModel, Cmd.none )

        CountryBorderHelperOutlineInactive ->
            ( GameLoaded gameLoadedModel, Cmd.none )

        CountryBorderHelperOutlineWaitingForDelay countryToShowInfoForId ->
            if clickedCountryId == countryToShowInfoForId then
                case LocalGame.countryClicked clickedCountryId gameLoadedModel.activeGame of
                    Ok updatedGame ->
                        ( GameSaving
                            { gameLoadedModel
                                | activeGame = updatedGame
                                , countryBorderHelperOutlineStatus = CountryBorderHelperOutlineInactive
                            }
                            RemoteData.Loading
                        , LocalGame.save apiUrl updatedGame GotGame
                        )

                    Err error ->
                        ( GameLoaded { gameLoadedModel | error = Just (LocalGame.errorToString error) }, Cmd.none )

            else
                ( GameLoaded gameLoadedModel, Cmd.none )


handleCountryMouseDown : Country.Id -> GameLoadedModel -> GameLoadedModel
handleCountryMouseDown countryId activeGame =
    { activeGame | countryBorderHelperOutlineStatus = CountryBorderHelperOutlineWaitingForDelay countryId }


handleCountryMouseOut : Country.Id -> GameLoadedModel -> GameLoadedModel
handleCountryMouseOut mouseOutCountryId activeGame =
    case activeGame.countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineWaitingForDelay countryId ->
            if countryId == mouseOutCountryId then
                { activeGame | countryBorderHelperOutlineStatus = CountryBorderHelperOutlineInactive }

            else
                activeGame

        _ ->
            activeGame


waitingToShowCountryHelperOutlines : CountryBorderHelperOutlineStatus -> Bool
waitingToShowCountryHelperOutlines countryBorderHelperOutlineStatus =
    case countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineWaitingForDelay _ ->
            True

        _ ->
            False



---- VIEW ----


viewGameWrapMessage : GameLoadedModel -> (Msg -> msg) -> { title : String, content : Html.Html msg }
viewGameWrapMessage model toMsg =
    let
        { title, content } =
            viewGame model

        updatedContent =
            content |> Html.map toMsg
    in
    { title = title, content = updatedContent }


viewGame : GameLoadedModel -> { title : String, content : Html.Html Msg }
viewGame gameLoaded =
    { content =
        let
            device =
                Element.classifyDevice gameLoaded.session.windowSize
        in
        case Element.classifyDevice gameLoaded.session.windowSize |> .class of
            Element.Phone ->
                viewPlayingGameMobile gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device

            _ ->
                viewPlayingGameDesktop gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device
    , title = "Fracas"
    }


view : Model -> { title : String, content : Html.Html Msg }
view model =
    case model of
        GameLoading gameLoading ->
            { title = "Fracas - Loading"
            , content =
                Element.layout []
                    (case gameLoading.error of
                        Just error ->
                            Element.text error

                        Nothing ->
                            Element.none
                    )
            }

        GameLoaded gameLoaded ->
            viewGame gameLoaded

        GameSaving gameLoaded _ ->
            { content =
                let
                    device =
                        Element.classifyDevice gameLoaded.session.windowSize
                in
                case Element.classifyDevice gameLoaded.session.windowSize |> .class of
                    Element.Phone ->
                        viewPlayingGameMobile gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device

                    _ ->
                        viewPlayingGameDesktop gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device
            , title = "Fracas"
            }


stopShowingCountryHelperOutlines : GameLoadedModel -> GameLoadedModel
stopShowingCountryHelperOutlines activeGame =
    { activeGame | countryBorderHelperOutlineStatus = CountryBorderHelperOutlineInactive }


makeCountryHelperOutlinesActive : GameLoadedModel -> GameLoadedModel
makeCountryHelperOutlinesActive model =
    case model.countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineWaitingForDelay countryId ->
            { model | countryBorderHelperOutlineStatus = CountryBorderHelperOutlineActive countryId }

        _ ->
            model


countryBorderColor : Colors.Color
countryBorderColor =
    Colors.rgb255 100 100 100


viewPlayingGameMobile : LocalGame.Game -> Bool -> CountryBorderHelperOutlineStatus -> Maybe String -> Element.Device -> Html.Html Msg
viewPlayingGameMobile activeGame showAvailableMoves countryBorderHelperOutlineStatus maybeError device =
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
                    ((case maybeError of
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
                    (getGameBoardHtml 100 activeGame showAvailableMoves countryBorderHelperOutlineStatus device |> Element.html)
                , viewInfoPanelPhone activeGame showAvailableMoves countryBorderHelperOutlineStatus
                ]
            ]
        )


viewPlayingGameDesktop : LocalGame.Game -> Bool -> CountryBorderHelperOutlineStatus -> Maybe String -> Element.Device -> Html.Html Msg
viewPlayingGameDesktop activeGame showAvailableMoves countryBorderHelperOutlineStatus maybeError device =
    Element.layout [ Element.width Element.fill, Element.Events.onMouseUp MouseUp ]
        (Element.row
            [ Element.centerX, Element.width Element.fill ]
            [ viewInfoPanelDesktop activeGame showAvailableMoves countryBorderHelperOutlineStatus
            , Element.column
                [ Element.centerX
                , Element.width Element.fill
                , Element.alignTop
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    (getGameBoardHtml 100 activeGame showAvailableMoves countryBorderHelperOutlineStatus device |> Element.html)
                , Element.column
                    [ Element.width Element.fill
                    , Element.Border.width 1
                    , Element.Border.color black
                    , Element.Border.solid
                    ]
                    ((case maybeError of
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


viewInfoPanelPhone : LocalGame.Game -> Bool -> CountryBorderHelperOutlineStatus -> Element.Element Msg
viewInfoPanelPhone activeGame showAvailableMoves countryBorderHelperOutlineStatus =
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
        , viewCountryInfo activeGame countryBorderHelperOutlineStatus
        , viewShowAvailableMoves showAvailableMoves
        ]


viewInfoPanelDesktop : LocalGame.Game -> Bool -> CountryBorderHelperOutlineStatus -> Element.Element Msg
viewInfoPanelDesktop activeGame showAvailableMoves countryBorderHelperOutlineStatus =
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
        , viewCountryInfo activeGame countryBorderHelperOutlineStatus
        , viewShowAvailableMoves showAvailableMoves
        ]


viewShowAvailableMoves : Bool -> Element.Element Msg
viewShowAvailableMoves showAvailableMoves =
    Element.row
        [ Element.alignBottom ]
        [ Element.Input.checkbox
            []
            { label =
                Element.Input.labelRight [ Element.Font.size 12, Element.Font.color (Colors.white |> Colors.toElementColor) ]
                    (Element.text "Show available moves")
            , icon = Element.Input.defaultCheckbox
            , checked = showAvailableMoves
            , onChange = ShowAvailableMovesCheckboxToggled
            }
        ]


viewPassButtonIfNecessary : PlayerTurn.PlayerTurn -> Element.Element Msg
viewPassButtonIfNecessary currentPlayerTurn =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 50)
        ]
        (if PlayerTurn.canCurrentPlayerPass currentPlayerTurn then
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
    Colors.darkGray |> ViewHelpers.colorToElementColor


viewPlayerCountryAndTroopCounts : PlayerTurn.PlayerTurn -> Dict.Dict String Player.Player -> Element.Element Msg
viewPlayerCountryAndTroopCounts currentPlayerTurn players =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (LocalGame.getPlayerCountryAndTroopCounts { currentPlayerTurn = currentPlayerTurn, players = players }
            |> List.map (viewPlayerTroopCount (PlayerTurn.getCurrentPlayer currentPlayerTurn) players)
        )


viewPlayerCountryAndTroopCountsMobile : PlayerTurn.PlayerTurn -> Dict.Dict String Player.Player -> Element.Element Msg
viewPlayerCountryAndTroopCountsMobile currentPlayerTurn players =
    Element.wrappedRow
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (LocalGame.getPlayerCountryAndTroopCounts { currentPlayerTurn = currentPlayerTurn, players = players }
            |> List.map (viewPlayerTroopCount (PlayerTurn.getCurrentPlayer currentPlayerTurn) players)
        )


attackerInfo : Player.Id -> LocalGame.Game -> Dict.Dict String TroopCount.TroopCount -> Element.Element Msg
attackerInfo countyOwnerPlayerId activeGame attackerStrengthPerPlayer =
    Element.column
        [ Element.width Element.fill, Element.spacing 3 ]
        (attackerStrengthPerPlayer
            |> Dict.toList
            |> List.filter
                (\( playerId, troopCount ) ->
                    Player.Id playerId /= countyOwnerPlayerId && troopCount /= TroopCount.noTroops
                )
            |> List.map
                (\( playerId, troopCount ) ->
                    case Player.getPlayer (Player.Id playerId) activeGame.players of
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


viewCountryInfo : LocalGame.Game -> CountryBorderHelperOutlineStatus -> Element.Element Msg
viewCountryInfo activeGame countryBorderHelperOutlineStatus =
    case countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineActive countryToShowInfoForId ->
            case LocalGame.findCountryOwner countryToShowInfoForId activeGame.players of
                Just playerId ->
                    case Player.getPlayer playerId activeGame.players of
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
                                    , Element.Border.color (Colors.lightGreen |> ViewHelpers.colorToElementColor)
                                    , Element.Border.solid
                                    , Element.Border.width 3
                                    ]
                                    [ Element.el [] (Element.text "Defense")
                                    , Element.el
                                        [ Element.alignRight ]
                                        (LocalGame.getCountryDefenseStrength activeGame.map activeGame.players countryToShowInfoForId |> TroopCount.toString |> Element.text)
                                    ]
                                , Element.column
                                    [ Element.width Element.fill
                                    , Element.Font.size 14
                                    , Element.padding 3
                                    , Element.spacing 3
                                    , Element.Border.color (Colors.red |> ViewHelpers.colorToElementColor)
                                    , Element.Border.solid
                                    , Element.Border.width 3
                                    ]
                                    [ Element.el
                                        [ Element.width Element.fill ]
                                        (Element.text "Opponent attack")
                                    , LocalGame.getAttackStrengthPerPlayer activeGame.map activeGame.players countryToShowInfoForId
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
    Player.Id
    -> Player.Players
    -> { playerId : Player.Id, countryCount : Int, troopCount : TroopCount.TroopCount, isAlive : Bool }
    -> Element.Element Msg
viewPlayerTroopCount currentPlayerId players status =
    let
        fontColor =
            if status.isAlive then
                black

            else
                Colors.darkGray |> ViewHelpers.colorToElementColor
    in
    case Player.getPlayer status.playerId players of
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
                    [ Element.Background.color (Colors.lightGray |> ViewHelpers.colorToElementColor)
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


playerAndTroopCountBorder : Player.Id -> Player.Id -> List (Element.Attribute Msg)
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


black : Element.Color
black =
    Element.rgb255 0 0 0


defaultLabelAttributes : List (Element.Attribute msg)
defaultLabelAttributes =
    [ Element.Font.size 12
    ]


viewConfigureTroopCountIfNecessary : PlayerTurn.PlayerTurn -> Element.Element Msg
viewConfigureTroopCountIfNecessary currentPlayerTurn =
    Element.el
        [ Element.width Element.fill
        ]
        (case currentPlayerTurn |> PlayerTurn.troopsToMove of
            Just numberOfTroopsToMove ->
                Element.column
                    [ Element.width Element.fill
                    , Element.padding 10
                    ]
                    [ Element.Input.text
                        (ViewHelpers.defaultTextInputAttributes ++ [ Element.alignLeft ])
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


viewPlayerTurnStatus : Int -> Int -> PlayerTurn.PlayerTurn -> Dict.Dict String Player.Player -> Element.Element Msg
viewPlayerTurnStatus height fontSize playerTurn players =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px height)
        , Element.Background.color (LocalGame.getPlayerColorFromPlayerTurn players playerTurn |> ViewHelpers.colorToElementColor)
        , Element.padding 5
        ]
        (Element.el
            [ Element.width Element.fill ]
            (Element.paragraph [ Element.Font.size fontSize ]
                [ Element.text
                    (PlayerTurn.toString players playerTurn)
                ]
            )
        )


getWaterCollage : Map.Map -> Collage.Collage Msg
getWaterCollage gameMap =
    let
        background =
            Collage.polygon
                [ ( 0, 0 )
                , ( 0, gameMap.dimensions |> Tuple.second |> toFloat )
                , ( gameMap.dimensions |> Tuple.first |> toFloat, gameMap.dimensions |> Tuple.second |> toFloat )
                , ( gameMap.dimensions |> Tuple.first |> toFloat, 0.0 )
                ]

        backgroundWater =
            background
                |> Collage.filled (Collage.uniform (Colors.blue |> Colors.toColor))

        backgroundBorder =
            background
                |> Collage.outlined (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 8.0) (Collage.uniform (Colors.black |> Colors.toColor)))
    in
    Collage.group [ backgroundBorder, backgroundWater ]


getGameBoardHtml : Int -> LocalGame.Game -> Bool -> CountryBorderHelperOutlineStatus -> Element.Device -> Html.Html Msg
getGameBoardHtml scaleFactor activeGame showAvailableMoves countryBorderHelperOutlineStatus device =
    case LocalGame.getCountriesToRender activeGame.map activeGame.players activeGame.currentPlayerTurn activeGame.neutralCountryTroops of
        Just countriesToRender ->
            let
                waterCollage : Collage.Collage Msg
                waterCollage =
                    Map.getWaterCollage scaleFactor activeGame.map.dimensions

                countriesCollage =
                    countriesToRender
                        |> List.map getCountryCollage
                        |> Collage.group

                troopCountFontSize =
                    case device.class of
                        Element.Phone ->
                            scaleFactor * 2

                        _ ->
                            toFloat scaleFactor * 0.9 |> round

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

                availableMoves =
                    if showAvailableMoves then
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
                        |> List.map (getCountryInfoPolygonBorder activeGame.map activeGame.players countryBorderHelperOutlineStatus)
                        |> Collage.group
            in
            Collage.group
                [ countryEventHandlers
                , countryInfoHighlights
                , availableMoves
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


countryHighlight : LocalGame.CountryToRender -> Collage.Collage Msg
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


getEventHandlersForCountry : LocalGame.CountryToRender -> Collage.Collage Msg
getEventHandlersForCountry countryToRender =
    countryToRender.polygonPoints
        |> Collage.polygon
        |> Collage.filled (Colors.transparency 0 |> Collage.uniform)
        |> Collage.Events.onMouseUp (\_ -> CountryMouseUp countryToRender.id)
        |> Collage.Events.onMouseDown (\_ -> CountryMouseDown countryToRender.id)
        |> Collage.Events.onMouseLeave (\_ -> CountryMouseOut countryToRender.id)


getPortCollage : LocalGame.CountryToRender -> Collage.Collage Msg
getPortCollage countryToRender =
    (case countryToRender.portSegments of
        Just portSegments ->
            [ renderPort portSegments ]

        Nothing ->
            []
    )
        |> Collage.group


countryHighlightCollage : Float -> LocalGame.CountryToRender -> Collage.Collage Msg
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
                (Collage.uniform (countryCanBeClickedColor |> Colors.toColor))
            )


countryCanBeClickedColor : Colors.Color
countryCanBeClickedColor =
    Colors.white


getGrayedOutCountryCollage : LocalGame.CountryToRender -> Collage.Collage Msg
getGrayedOutCountryCollage countryToRender =
    countryToRender.polygonPoints
        |> Collage.polygon
        |> Collage.filled (Colors.transparency 0.5 |> Collage.uniform)


getTroopCountCollage : Int -> LocalGame.CountryToRender -> Collage.Collage Msg
getTroopCountCollage fontSize countryToRender =
    if TroopCount.hasTroops countryToRender.troopCount then
        countryToRender.troopCount
            |> TroopCount.toString
            |> Collage.Text.fromString
            |> Collage.Text.color (Colors.black |> Colors.toColor)
            |> Collage.Text.size fontSize
            |> Collage.rendered
            |> Collage.shift countryToRender.center

    else
        Collage.group []


getCountryCollage : LocalGame.CountryToRender -> Collage.Collage Msg
getCountryCollage countryToRender =
    let
        countryPolygon =
            Collage.polygon countryToRender.polygonPoints

        fill =
            countryPolygon
                |> Collage.filled (Collage.uniform (countryToRender.color |> Colors.toColor))

        border =
            countryPolygon
                |> Collage.outlined
                    (Collage.solid 30.0
                        (Collage.uniform (countryBorderColor |> Colors.toColor))
                    )
    in
    Collage.group [ fill, border ]


renderCapitolDots : LocalGame.CountryToRender -> Collage.Collage Msg
renderCapitolDots countryToRender =
    let
        ( capitolDot, capitolDotsCoords ) =
            case countryToRender.capitolDots of
                Just capitolDots ->
                    ( [ Collage.square (toFloat ViewHelpers.pixelsPerMapSquare / 10.0)
                            |> Collage.filled (Collage.uniform (Colors.black |> Colors.toColor))
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


getCountryInfoPolygonBorder : Map.Map -> Player.Players -> CountryBorderHelperOutlineStatus -> LocalGame.CountryToRender -> Collage.Collage Msg
getCountryInfoPolygonBorder gameMap players countryBorderHelperOutlineStatus countryToRender =
    case getCountryInfoStatus gameMap players countryBorderHelperOutlineStatus countryToRender.id of
        CountryInfoSelectedCountry ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 6.0)
                        (Collage.uniform (Colors.white |> Colors.toColor))
                    )

        CountryInfoDefending ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 6.0)
                        (Collage.uniform (Colors.green |> Colors.toColor))
                    )

        CountryInfoAttacking ->
            Collage.polygon countryToRender.polygonPoints
                |> Collage.outlined
                    (Collage.solid (toFloat ViewHelpers.pixelsPerMapSquare / 6.0)
                        (Collage.uniform (Colors.red |> Colors.toColor))
                    )

        NoInfo ->
            Collage.group []


type CountryInfoStatus
    = CountryInfoSelectedCountry
    | CountryInfoDefending
    | CountryInfoAttacking
    | NoInfo


getCountryInfoStatus : Map.Map -> Player.Players -> CountryBorderHelperOutlineStatus -> Country.Id -> CountryInfoStatus
getCountryInfoStatus gameMap players countryBorderHelperOutlineStatus countryId =
    case countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineActive countryToShowInfoForId ->
            if countryToShowInfoForId == countryId then
                CountryInfoSelectedCountry

            else if LocalGame.isCountryDefending gameMap players countryToShowInfoForId countryId then
                CountryInfoDefending

            else if LocalGame.isCountryAttacking gameMap players countryToShowInfoForId countryId then
                CountryInfoAttacking

            else
                NoInfo

        _ ->
            NoInfo


renderPort : Set.Set ( Country.ScaledPoint, Country.ScaledPoint ) -> Collage.Collage msg
renderPort waterEdges =
    waterEdges
        |> Set.toList
        |> List.map
            (\( point1, point2 ) ->
                Collage.segment point1 point2
                    |> Collage.traced
                        (Collage.broken [ ( 3, 10 ) ]
                            ((ViewHelpers.pixelsPerMapSquare |> toFloat) / 2.0)
                            (Collage.uniform (Colors.gray |> Colors.toColor))
                        )
            )
        |> Collage.group



-- Subscriptions


countryOutlineDelayMilliseconds : Float
countryOutlineDelayMilliseconds =
    300


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        GameLoaded gameLoadedModel ->
            [ if waitingToShowCountryHelperOutlines gameLoadedModel.countryBorderHelperOutlineStatus then
                Time.every countryOutlineDelayMilliseconds (always ShowCountryBorderHelper)

              else
                Sub.none
            , Browser.Events.onResize (\x y -> WindowResized x y)
            ]
                |> Sub.batch

        GameLoading _ ->
            Browser.Events.onResize (\x y -> WindowResized x y)

        GameSaving _ _ ->
            Browser.Events.onResize (\x y -> WindowResized x y)
