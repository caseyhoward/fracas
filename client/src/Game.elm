module Game exposing
    ( CountryBorderHelperOutlineStatus(..)
    , Error
    , Game
    , GameLoadedModel
    , GameWithCurrentUser
    , Id(..)
    , Model(..)
    , Msg(..)
    , countryClicked
    ,  handleCountryMouseUpFromPlayer
       -- , init

    , idToString
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
import Map
import Player
import PlayerTurn
import RemoteData
import Session
import Set
import Time
import TroopCount
import Url.Parser
import ViewHelpers


urlParser : Url.Parser.Parser (Id -> a) a
urlParser =
    Url.Parser.custom "GAMEID" (\str -> Just (Id str))


idToString : Id -> String
idToString (Id id) =
    id


type Model
    = GameLoading GameLoadingModel
    | GameLoaded GameLoadedModel
    | GameSaving GameLoadedModel (RemoteData.RemoteData (Graphql.Http.Error Game) Game)


type alias GameLoadingModel =
    { activeGame : RemoteData.RemoteData (Graphql.Http.Error Game) Game
    , error : Maybe String
    , session : Session.Session
    , playerId : Player.Id
    }


type alias GameLoadedModel =
    { activeGame : Game
    , showAvailableMoves : Bool
    , session : Session.Session
    , error : Maybe String
    , playerId : Player.Id
    , countryBorderHelperOutlineStatus : CountryBorderHelperOutlineStatus
    }


type alias Game =
    { id : Id
    , currentPlayerTurn : PlayerTurn.PlayerTurn
    , map : Map.Map
    , players : Player.Players
    , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
    }



type Id
    = Id String


type alias GameWithCurrentUser =
    { game : Game
    , currentUserPlayerId : Player.Id
    }


type CountryBorderHelperOutlineStatus
    = CountryBorderHelperOutlineWaitingForDelay Country.Id
    | CountryBorderHelperOutlineInactive
    | CountryBorderHelperOutlineActive Country.Id



-- init : Session.Session -> Id -> Player.Id -> ( Model, Cmd Msg )
-- init session activeGameId playerId =
--     ( GameLoading
--         { activeGame = RemoteData.NotAsked
--         , error = Nothing
--         , playerId = playerId
--         , session = session
--         }
--     , get session.apiUrl activeGameId GotGame
--     )


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
    | GotGame (RemoteData.RemoteData (Graphql.Http.Error Game) Game)
    | MouseUp
    | Pass
    | UpdateNumberOfTroopsToMove String
    | CancelMovingTroops
    | ShowCountryBorderHelper
    | ShowAvailableMovesCheckboxToggled Bool
    -- | WindowResized Int Int




update : Msg -> Model -> ( Model, Cmd msg )
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


                _ ->
                    ( model, Cmd.none )

        GameLoaded gameLoadedModel ->
            case msg of
                CountryMouseUp clickedCountryId ->
                    ( GameLoaded (handleCountryMouseUpFromPlayer clickedCountryId gameLoadedModel), Cmd.none )

                CountryMouseDown clickedCountryId ->
                    ( handleCountryMouseDown clickedCountryId gameLoadedModel |> GameLoaded, Cmd.none )

                CountryMouseOut mouseOutCountryId ->
                    ( handleCountryMouseOut mouseOutCountryId gameLoadedModel |> GameLoaded, Cmd.none )

                ShowAvailableMovesCheckboxToggled isChecked ->
                    ( GameLoaded { gameLoadedModel | showAvailableMoves = isChecked }, Cmd.none )

                MouseUp ->
                    ( stopShowingCountryHelperOutlines gameLoadedModel |> GameLoaded, Cmd.none )

                Pass ->
                    ( updateModelWithGameResult (pass gameLoadedModel.activeGame) gameLoadedModel |> GameLoaded, Cmd.none )

                UpdateNumberOfTroopsToMove numberOfTroopsToMoveString ->
                    ( GameLoaded { gameLoadedModel | activeGame = updateNumberOfTroopsToMove numberOfTroopsToMoveString gameLoadedModel.activeGame }, Cmd.none )

                CancelMovingTroops ->
                    ( GameLoaded { gameLoadedModel | activeGame = cancelMovingTroops gameLoadedModel.activeGame }, Cmd.none )

                ShowCountryBorderHelper ->
                    ( makeCountryHelperOutlinesActive gameLoadedModel |> GameLoaded, Cmd.none )

                GotGame _ ->
                    ( model, Cmd.none )


updateModelWithGameResult : Result Error Game -> GameLoadedModel -> GameLoadedModel
updateModelWithGameResult result model =
    case result of
        Ok activeGame ->
            { model | activeGame = activeGame, error = Nothing }

        Err error ->
            { model | error = Just (errorToString error) }


handleCountryMouseUpFromPlayer : Country.Id -> GameLoadedModel -> GameLoadedModel
handleCountryMouseUpFromPlayer clickedCountryId gameLoadedModel  =
    case gameLoadedModel.countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineActive _ ->
            gameLoadedModel

        CountryBorderHelperOutlineInactive ->
            gameLoadedModel

        CountryBorderHelperOutlineWaitingForDelay countryToShowInfoForId ->
            if clickedCountryId == countryToShowInfoForId then
                gameLoadedModel
                -- case countryClicked clickedCountryId gameLoadedModel.activeGame of
                --     Ok updatedGame ->
                --         ( GameSaving
                --             { gameLoadedModel
                --                 | activeGame = updatedGame
                --                 , countryBorderHelperOutlineStatus = CountryBorderHelperOutlineInactive
                --             }
                --             RemoteData.Loading
                --         , save apiUrl updatedGame GotGame
                --         )
                -- Err error ->
                --     ( GameLoaded { gameLoadedModel | error = Just (errorToString error) }, Cmd.none )

            else
                gameLoadedModel


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
        case gameLoaded.session.windowSize of
            Just windowSize ->
                let
                    device =
                        Element.classifyDevice windowSize
                in
                case Element.classifyDevice windowSize |> .class of
                    Element.Phone ->
                        viewPlayingGameMobile gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device

                    _ ->
                        viewPlayingGameDesktop gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device

            Nothing ->
                viewPlayingGameDesktop gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error (Element.classifyDevice { width = 1920, height = 1080 })
    , title = "Fracas"
    }


view : Model -> (Msg -> msg) -> { title : String, content : Html.Html msg }
view model toMsg =
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
                    |> Html.map toMsg
            }

        GameLoaded gameLoaded ->
            let
                { title, content } =
                    viewGame gameLoaded
            in
            { title = title, content = content |> Html.map toMsg }

        GameSaving gameLoaded _ ->
            { content =
                (case gameLoaded.session.windowSize of
                    Just windowSize ->
                        let
                            device =
                                Element.classifyDevice windowSize
                        in
                        case Element.classifyDevice windowSize |> .class of
                            Element.Phone ->
                                viewPlayingGameMobile gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device

                            _ ->
                                viewPlayingGameDesktop gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device

                    Nothing ->
                        viewPlayingGameDesktop gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error (Element.classifyDevice { width = 1920, height = 1080 })
                )
                    |> Html.map toMsg
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


viewPlayingGameMobile : Game -> Bool -> CountryBorderHelperOutlineStatus -> Maybe String -> Element.Device -> Html.Html Msg
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


viewPlayingGameDesktop : Game -> Bool -> CountryBorderHelperOutlineStatus -> Maybe String -> Element.Device -> Html.Html Msg
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


viewInfoPanelPhone : Game -> Bool -> CountryBorderHelperOutlineStatus -> Element.Element Msg
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


viewInfoPanelDesktop : Game -> Bool -> CountryBorderHelperOutlineStatus -> Element.Element Msg
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
                Element.Input.labelRight [ Element.Font.size 12 ]
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
        (getPlayerCountryAndTroopCounts { currentPlayerTurn = currentPlayerTurn, players = players }
            |> List.map (viewPlayerTroopCount (PlayerTurn.getCurrentPlayer currentPlayerTurn) players)
        )


viewPlayerCountryAndTroopCountsMobile : PlayerTurn.PlayerTurn -> Dict.Dict String Player.Player -> Element.Element Msg
viewPlayerCountryAndTroopCountsMobile currentPlayerTurn players =
    Element.wrappedRow
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (getPlayerCountryAndTroopCounts { currentPlayerTurn = currentPlayerTurn, players = players }
            |> List.map (viewPlayerTroopCount (PlayerTurn.getCurrentPlayer currentPlayerTurn) players)
        )


attackerInfo : Player.Id -> Game -> Dict.Dict String TroopCount.TroopCount -> Element.Element Msg
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


viewCountryInfo : Game -> CountryBorderHelperOutlineStatus -> Element.Element Msg
viewCountryInfo activeGame countryBorderHelperOutlineStatus =
    case countryBorderHelperOutlineStatus of
        CountryBorderHelperOutlineActive countryToShowInfoForId ->
            case findCountryOwner countryToShowInfoForId activeGame.players of
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
                                        (getCountryDefenseStrength activeGame.map activeGame.players countryToShowInfoForId |> TroopCount.toString |> Element.text)
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
                                    , getAttackStrengthPerPlayer activeGame.map activeGame.players countryToShowInfoForId
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
        , Element.Background.color (getPlayerColorFromPlayerTurn players playerTurn |> ViewHelpers.colorToElementColor)
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


getGameBoardHtml : Int -> Game -> Bool -> CountryBorderHelperOutlineStatus -> Element.Device -> Html.Html Msg
getGameBoardHtml scaleFactor activeGame showAvailableMoves countryBorderHelperOutlineStatus device =
    case getCountriesToRender activeGame.map activeGame.players activeGame.currentPlayerTurn activeGame.neutralCountryTroops of
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


countryHighlight : CountryToRender -> Collage.Collage Msg
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


getEventHandlersForCountry : CountryToRender -> Collage.Collage Msg
getEventHandlersForCountry countryToRender =
    countryToRender.polygonPoints
        |> Collage.polygon
        |> Collage.filled (Colors.transparency 0 |> Collage.uniform)
        |> Collage.Events.onMouseUp (\_ -> CountryMouseUp countryToRender.id)
        |> Collage.Events.onMouseDown (\_ -> CountryMouseDown countryToRender.id)
        |> Collage.Events.onMouseLeave (\_ -> CountryMouseOut countryToRender.id)


getPortCollage : CountryToRender -> Collage.Collage Msg
getPortCollage countryToRender =
    (case countryToRender.portSegments of
        Just portSegments ->
            [ renderPort portSegments ]

        Nothing ->
            []
    )
        |> Collage.group


countryHighlightCollage : Float -> CountryToRender -> Collage.Collage Msg
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


getGrayedOutCountryCollage : CountryToRender -> Collage.Collage Msg
getGrayedOutCountryCollage countryToRender =
    countryToRender.polygonPoints
        |> Collage.polygon
        |> Collage.filled (Colors.transparency 0.5 |> Collage.uniform)


getTroopCountCollage : Int -> CountryToRender -> Collage.Collage Msg
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


getCountryCollage : CountryToRender -> Collage.Collage Msg
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


renderCapitolDots : CountryToRender -> Collage.Collage Msg
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


getCountryInfoPolygonBorder : Map.Map -> Player.Players -> CountryBorderHelperOutlineStatus -> CountryToRender -> Collage.Collage Msg
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

            else if isCountryDefending gameMap players countryToShowInfoForId countryId then
                CountryInfoDefending

            else if isCountryAttacking gameMap players countryToShowInfoForId countryId then
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
    Sub.none


-------------------------------------------


errorToString : Error -> String
errorToString (Error error) =
    error


countryClicked : Country.Id -> Game -> Result Error Game
countryClicked clickedCountryId activeGame =
    -- if PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn userId then
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn playerTurnStage currentPlayerId ->
            case playerTurnStage of
                PlayerTurn.CapitolPlacement ->
                    attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame

                PlayerTurn.TroopPlacement ->
                    attemptTroopPlacement clickedCountryId currentPlayerId (Player.numberOfTroopsToPlace currentPlayerId activeGame.players) activeGame

                PlayerTurn.AttackAnnexOrPort ->
                    attackAnnexOrPort clickedCountryId currentPlayerId activeGame

                PlayerTurn.TroopMovement ->
                    attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame

                PlayerTurn.TroopMovementFromSelected fromCountryId numberOfTroopsToMoveString ->
                    attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame

                PlayerTurn.GameOver ->
                    Ok activeGame



-- else
--     "Not your turn" |> Error |> Err


getAttackStrengthPerPlayer : Map.Map -> Player.Players -> Country.Id -> Dict.Dict String TroopCount.TroopCount
getAttackStrengthPerPlayer gameMap players countryId =
    getCountryAttackers gameMap players countryId
        |> Dict.map
            (\_ attacker ->
                let
                    neighborAttack =
                        attacker.neighboringCountryAttackers
                            |> Dict.foldl
                                (\_ troopCount result ->
                                    TroopCount.addTroopCounts troopCount result
                                )
                                TroopCount.noTroops

                    waterAttack =
                        attacker.neighboringThroughWaterAttackers
                            |> Dict.foldl
                                (\_ troopCount result ->
                                    TroopCount.addTroopCounts (troopCount |> TroopCount.acrossWater) result
                                )
                                TroopCount.noTroops
                in
                TroopCount.addTroopCounts neighborAttack waterAttack
            )


getCountryAttackers : Map.Map -> Player.Players -> Country.Id -> CountryAttackers
getCountryAttackers gameMap players countryId =
    let
        neighborCountriesByPlayer : List ( Player.Id, Country.Id )
        neighborCountriesByPlayer =
            case Country.getCountry countryId gameMap.countries of
                Just country ->
                    country.neighboringCountries
                        |> Set.foldl
                            (\neighborCountry result ->
                                case findCountryOwner (Country.Id neighborCountry) players of
                                    Just neighborId ->
                                        ( neighborId, Country.Id neighborCountry ) :: result

                                    Nothing ->
                                        result
                            )
                            []

                Nothing ->
                    []

        neighborCountryTroopCountsByPlayer : List ( Player.Id, Country.Id, TroopCount.TroopCount )
        neighborCountryTroopCountsByPlayer =
            neighborCountriesByPlayer
                |> List.map
                    (\( playerId, neighborCountryId ) ->
                        ( playerId
                        , neighborCountryId
                        , getTroopCountForCountry neighborCountryId players |> Maybe.withDefault TroopCount.noTroops
                        )
                    )

        neighboringCountryAttackers : Dict.Dict String (Dict.Dict String TroopCount.TroopCount)
        neighboringCountryAttackers =
            neighborCountryTroopCountsByPlayer
                |> List.foldl
                    (\( Player.Id playerId, Country.Id neighborCountryId, troopCount ) result ->
                        case Dict.get playerId result of
                            Just troopCounts ->
                                result |> Dict.insert playerId (Dict.insert neighborCountryId troopCount troopCounts)

                            Nothing ->
                                result
                                    |> Dict.insert playerId
                                        (Dict.fromList
                                            [ ( neighborCountryId, troopCount ) ]
                                        )
                    )
                    Dict.empty

        countriesReachableThroughWater : List Country.Id
        countriesReachableThroughWater =
            Map.getCountriesThatCanReachCountryThroughWater gameMap.countries gameMap.bodiesOfWater countryId

        attackerCountriesNeighoboringWater : List Country.Id
        attackerCountriesNeighoboringWater =
            countriesReachableThroughWater

        attackerCountriesNeighoboringWaterWithPort : List Country.Id
        attackerCountriesNeighoboringWaterWithPort =
            attackerCountriesNeighoboringWater
                |> filterCountriesWithPort players

        waterNeighborCountriesByPlayer : List ( Player.Id, Country.Id )
        waterNeighborCountriesByPlayer =
            attackerCountriesNeighoboringWaterWithPort
                |> List.foldl
                    (\waterNeighborCountry result ->
                        case findCountryOwner waterNeighborCountry players of
                            Just neighborId ->
                                ( neighborId, waterNeighborCountry ) :: result

                            Nothing ->
                                result
                    )
                    []

        waterNeighborCountriesByPlayerTroopCounts : List ( Player.Id, Country.Id, TroopCount.TroopCount )
        waterNeighborCountriesByPlayerTroopCounts =
            waterNeighborCountriesByPlayer
                |> List.map
                    (\( playerId, neighborCountryId ) ->
                        ( playerId
                        , neighborCountryId
                        , getTroopCountForCountry neighborCountryId players
                            |> Maybe.withDefault TroopCount.noTroops
                        )
                    )

        waterNeighborAttackers : Dict.Dict String (Dict.Dict String TroopCount.TroopCount)
        waterNeighborAttackers =
            waterNeighborCountriesByPlayerTroopCounts
                |> List.foldl
                    (\( Player.Id playerId, Country.Id neighborCountryId, troopCount ) result ->
                        case Dict.get playerId result of
                            Just troopCounts ->
                                result |> Dict.insert playerId (Dict.insert neighborCountryId troopCount troopCounts)

                            Nothing ->
                                result
                                    |> Dict.insert playerId
                                        (Dict.fromList
                                            [ ( neighborCountryId, troopCount ) ]
                                        )
                    )
                    Dict.empty
    in
    players
        |> Dict.foldl
            (\playerId _ result ->
                result
                    |> Dict.insert
                        playerId
                        { neighboringCountryAttackers = Dict.get playerId neighboringCountryAttackers |> Maybe.withDefault Dict.empty
                        , neighboringThroughWaterAttackers = Dict.get playerId waterNeighborAttackers |> Maybe.withDefault Dict.empty
                        }
            )
            Dict.empty


type alias CountryToRender =
    { id : Country.Id
    , color : Colors.Color
    , troopCount : TroopCount.TroopCount
    , center : Country.ScaledPoint
    , polygonPoints : List Country.ScaledPoint
    , capitolDots : Maybe (Set.Set Country.ScaledPoint)
    , canBeClicked : Bool
    , isBeingMovedFrom : Bool
    , portSegments : Maybe (Set.Set ( Country.ScaledPoint, Country.ScaledPoint ))
    }


getCountriesToRender : Map.Map -> Player.Players -> PlayerTurn.PlayerTurn -> Dict.Dict String TroopCount.TroopCount -> Maybe (List CountryToRender)
getCountriesToRender gameMap players currentPlayerTurn neutralCountryTroops =
    gameMap.countries
        |> Country.scaledCountries ViewHelpers.pixelsPerMapSquare
        |> Dict.map
            (\countryId country ->
                let
                    countryOwnerAndTroopCount =
                        findCountryOwnerAndTroopCount (Country.Id countryId) players
                in
                case countryOwnerAndTroopCount of
                    Just ( countryOwnerId, troopCount ) ->
                        Player.getPlayer countryOwnerId players
                            |> Maybe.map
                                (\countryOwner ->
                                    { id = Country.Id countryId
                                    , troopCount = troopCount
                                    , center = country.center
                                    , polygonPoints = country.polygon
                                    , color = countryOwner.color
                                    , capitolDots =
                                        case countryOwner.capitolStatus of
                                            Player.Capitol (Country.Id capitolId) ->
                                                if capitolId == countryId then
                                                    Just country.coordinates

                                                else
                                                    Nothing

                                            Player.NoCapitol ->
                                                Nothing
                                    , canBeClicked = getCountryCanBeClicked currentPlayerTurn players gameMap (Country.Id countryId)
                                    , isBeingMovedFrom = getIsBeingMovedFrom currentPlayerTurn (Country.Id countryId)
                                    , portSegments =
                                        getCountryHasPort (Country.Id countryId) players
                                            |> Maybe.andThen
                                                (\hasPort ->
                                                    if hasPort then
                                                        Just country.waterEdges

                                                    else
                                                        Nothing
                                                )
                                    }
                                )

                    Nothing ->
                        Just
                            { id = Country.Id countryId
                            , troopCount = getTroopCount (Country.Id countryId) neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops
                            , center = country.center
                            , color = neutralCountryColor
                            , polygonPoints = country.polygon
                            , capitolDots = Nothing
                            , canBeClicked = getCountryCanBeClicked currentPlayerTurn players gameMap (Country.Id countryId)
                            , isBeingMovedFrom = False
                            , portSegments = Nothing
                            }
            )
        |> Dict.values
        |> List.foldl
            (\maybeCountryToRender result ->
                case ( result, maybeCountryToRender ) of
                    ( Just countriesToRender, Just countryToRender ) ->
                        Just (countryToRender :: countriesToRender)

                    _ ->
                        Nothing
            )
            (Just [])



-- Exposed


cancelMovingTroops : Game -> Game
cancelMovingTroops activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn _ playerId ->
            { activeGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId }


isCountryDefending : Map.Map -> Player.Players -> Country.Id -> Country.Id -> Bool
isCountryDefending gameMap players countryToDefend (Country.Id countryThatMightDefend) =
    let
        countryDefense =
            getCountryDefenders players gameMap countryToDefend

        defendingCountries =
            Dict.keys countryDefense.neighboringCountryDefense ++ Dict.keys countryDefense.neighboringThroughWaterDefense
    in
    defendingCountries |> Set.fromList |> Set.member countryThatMightDefend


isCountryAttacking : Map.Map -> Player.Players -> Country.Id -> Country.Id -> Bool
isCountryAttacking gameMap players countryToDefend (Country.Id countryThatMightDefend) =
    let
        countryAttackers =
            getCountryAttackers gameMap players countryToDefend

        attackingCountries : List String
        attackingCountries =
            countryAttackers
                |> Dict.foldl
                    (\_ attacker result ->
                        result ++ Dict.keys attacker.neighboringCountryAttackers ++ Dict.keys attacker.neighboringThroughWaterAttackers
                    )
                    []
    in
    attackingCountries |> Set.fromList |> Set.member countryThatMightDefend


getCountryDefenders : Player.Players -> Map.Map -> Country.Id -> CountryDefenders
getCountryDefenders players gameMap countryId =
    let
        playerId : Player.Id
        playerId =
            case findCountryOwner countryId players of
                Just ownerId ->
                    ownerId

                Nothing ->
                    Player.Id "-1"

        defendingCountryTroopCount : TroopCount.TroopCount
        defendingCountryTroopCount =
            case getTroopCountForCountry countryId players of
                Just countryBeingAttackedTroopCount ->
                    countryBeingAttackedTroopCount

                Nothing ->
                    -- This shouldn't happen
                    TroopCount.noTroops

        neigboringCountryDefense : Dict.Dict String TroopCount.TroopCount
        neigboringCountryDefense =
            case Country.getCountry countryId gameMap.countries of
                Just countryBeingAttacked ->
                    countryBeingAttacked.neighboringCountries
                        |> Set.toList
                        |> List.map (\id -> Country.Id id)
                        |> List.foldl
                            (\(Country.Id neighboringCountryId) defense ->
                                if isCountryOwnedByPlayer playerId (Country.Id neighboringCountryId) players then
                                    case getTroopCountForCountry (Country.Id neighboringCountryId) players of
                                        Just neighboringCountryTroopCount ->
                                            Dict.insert neighboringCountryId neighboringCountryTroopCount defense

                                        Nothing ->
                                            defense

                                else
                                    defense
                            )
                            Dict.empty

                Nothing ->
                    -- This shouldn't happen
                    Dict.empty

        defenseThroughWater =
            getDefenseThroughWater gameMap players countryId
                |> Dict.filter
                    (\throughWaterCountryId _ ->
                        case Country.getCountry countryId gameMap.countries of
                            Just countryBeingAttacked ->
                                not <| Set.member throughWaterCountryId countryBeingAttacked.neighboringCountries

                            Nothing ->
                                True
                    )
    in
    { neighboringCountryDefense = neigboringCountryDefense
    , neighboringThroughWaterDefense = defenseThroughWater
    , countryDefense = defendingCountryTroopCount
    }


getCountryDefenseStrength : Map.Map -> Player.Players -> Country.Id -> TroopCount.TroopCount
getCountryDefenseStrength gameMap players countryId =
    let
        countryDefense =
            getCountryDefenders players gameMap countryId

        neighborDefense =
            countryDefense.neighboringThroughWaterDefense
                |> Dict.values
                |> List.foldl (\troopCount result -> TroopCount.addTroopCounts (troopCount |> TroopCount.acrossWater) result) TroopCount.noTroops

        waterDefense =
            countryDefense.neighboringCountryDefense
                |> Dict.values
                |> List.foldl (\troopCount result -> TroopCount.addTroopCounts troopCount result) TroopCount.noTroops
    in
    countryDefense.countryDefense
        |> TroopCount.addTroopCounts neighborDefense
        |> TroopCount.addTroopCounts waterDefense


findCountryOwner : Country.Id -> Player.Players -> Maybe Player.Id
findCountryOwner countryId players =
    findCountryOwnerAndTroopCount countryId players
        |> Maybe.map Tuple.first


getCountryHasPort : Country.Id -> Player.Players -> Maybe Bool
getCountryHasPort (Country.Id countryId) players =
    findCountryOwner (Country.Id countryId) players
        |> Maybe.andThen
            (\playerId ->
                Player.getPlayer playerId players
                    |> Maybe.map .ports
                    |> Maybe.map
                        (Set.member countryId)
            )


getPlayerCountryAndTroopCounts :
    { players : Player.Players, currentPlayerTurn : PlayerTurn.PlayerTurn }
    -> List { playerId : Player.Id, countryCount : Int, troopCount : TroopCount.TroopCount, isAlive : Bool }
getPlayerCountryAndTroopCounts { players, currentPlayerTurn } =
    players
        |> Dict.map
            (\playerId player ->
                case player.capitolStatus of
                    Player.Capitol _ ->
                        { playerId = Player.Id playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = True
                        }

                    Player.NoCapitol ->
                        { playerId = Player.Id playerId
                        , countryCount = Dict.size player.countryTroopCounts
                        , troopCount = getTotalTroopCountForPlayer player
                        , isAlive = False || PlayerTurn.isCapitolPlacementTurn currentPlayerTurn
                        }
            )
        |> Dict.values


getTotalTroopCountForPlayer : Player.Player -> TroopCount.TroopCount
getTotalTroopCountForPlayer player =
    player.countryTroopCounts
        |> Dict.values
        |> List.foldl (\troopCount result -> TroopCount.addTroopCounts troopCount result) TroopCount.noTroops


getTroopCount : Country.Id -> Dict.Dict String TroopCount.TroopCount -> Maybe TroopCount.TroopCount
getTroopCount (Country.Id countryId) troopCounts =
    Dict.get countryId troopCounts


getTroopCountForCountry : Country.Id -> Player.Players -> Maybe TroopCount.TroopCount
getTroopCountForCountry countryId players =
    findCountryOwner countryId players
        |> Maybe.andThen (\playerId -> Player.getPlayer playerId players)
        |> Maybe.andThen (\player -> getTroopCount countryId player.countryTroopCounts)


getPlayerColorFromPlayerTurn : Player.Players -> PlayerTurn.PlayerTurn -> Colors.Color
getPlayerColorFromPlayerTurn players playerTurn =
    case playerTurn of
        PlayerTurn.PlayerTurn _ playerId ->
            Player.getPlayer playerId players
                |> Maybe.map
                    (\player ->
                        player.color
                    )
                |> Maybe.withDefault Colors.black


isCountryIdCapitol : Player.Id -> Country.Id -> Player.Players -> Maybe Bool
isCountryIdCapitol playerId countryId players =
    Player.getPlayer playerId players
        |> Maybe.map
            (\player ->
                case player.capitolStatus of
                    Player.Capitol capitolId ->
                        capitolId == countryId

                    Player.NoCapitol ->
                        False
            )


pass : Game -> Result Error Game
pass activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected _ _) playerId ->
            Ok { activeGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }

        PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId ->
            Ok { activeGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players) }

        PlayerTurn.PlayerTurn PlayerTurn.AttackAnnexOrPort playerId ->
            Ok
                { activeGame
                    | currentPlayerTurn =
                        if playerHasMoreThanOneCountry activeGame.players playerId then
                            PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId

                        else
                            PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                }

        _ ->
            "Can't pass" |> Error |> Err


updateNumberOfTroopsToMove : String -> Game -> Game
updateNumberOfTroopsToMove numberOfTroopsToMoveString activeGame =
    case activeGame.currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected countryId _) currentPlayerId ->
            { activeGame
                | currentPlayerTurn =
                    currentPlayerId |> PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected countryId numberOfTroopsToMoveString)
            }

        _ ->
            activeGame



------- LOCAL


type CountryStatus
    = Unoccupied
    | OccupiedByOpponent Player.Id
    | OccupiedByCurrentPlayer TroopCount.TroopCount


type AttackResult
    = CurrentPlayerAcquiresOpponentCountry
    | OpponentCountryLosesTroops TroopCount.TroopCount
    | OpponentEliminated
    | NotEnoughTroopsToAttack TroopCount.TroopCount TroopCount.TroopCount
    | AttackResultError String


type alias CountryDefenders =
    { neighboringCountryDefense : Dict.Dict String TroopCount.TroopCount
    , neighboringThroughWaterDefense : Dict.Dict String TroopCount.TroopCount
    , countryDefense : TroopCount.TroopCount
    }


type alias CountryAttackers =
    Dict.Dict String CountryAttackersForPlayer


type alias CountryAttackersForPlayer =
    { neighboringCountryAttackers : Dict.Dict String TroopCount.TroopCount
    , neighboringThroughWaterAttackers : Dict.Dict String TroopCount.TroopCount
    }


attackAnnexOrPort : Country.Id -> Player.Id -> Game -> Result Error Game
attackAnnexOrPort clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer _ ->
            attemptToBuildPort currentPlayerId clickedCountryId activeGame

        OccupiedByOpponent opponentPlayerId ->
            attemptToAttackCountry opponentPlayerId clickedCountryId activeGame

        Unoccupied ->
            attemptToAnnexCountry currentPlayerId clickedCountryId activeGame


attemptTroopMovement : Country.Id -> Country.Id -> String -> Game -> Result Error Game
attemptTroopMovement fromCountryId clickedCountryId numberOfTroopsToMoveString activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer playerCountryToTroopCount ->
            case String.toInt numberOfTroopsToMoveString of
                Just numberOfTroopsToMove ->
                    if isCountryReachableFromOtherCountry fromCountryId clickedCountryId activeGame.map.countries activeGame.players then
                        let
                            fromCountryTroopCount =
                                case Player.getPlayer (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) activeGame.players of
                                    Just currentPlayer1 ->
                                        case getTroopCount fromCountryId currentPlayer1.countryTroopCounts of
                                            Just troopCount ->
                                                troopCount

                                            Nothing ->
                                                TroopCount.noTroops

                                    Nothing ->
                                        TroopCount.noTroops

                            allowedNumberOfTroopsToMove =
                                TroopCount.numberOfTroopsToMove fromCountryTroopCount numberOfTroopsToMove

                            updatedGameResult =
                                activeGame
                                    |> updatePlayerTroopCountForCountry (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) fromCountryId (TroopCount.subtractTroopCounts allowedNumberOfTroopsToMove fromCountryTroopCount)
                                    |> Result.andThen (updatePlayerTroopCountForCountry (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) clickedCountryId (TroopCount.addTroopCounts playerCountryToTroopCount allowedNumberOfTroopsToMove))
                        in
                        updatedGameResult
                            |> Result.map
                                (\updatedGame ->
                                    { updatedGame
                                        | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn |> nextPlayerCheckForDeadPlayers activeGame.players)
                                    }
                                )

                    else
                        "You can't move troops between those countries" |> Error |> Err

                Nothing ->
                    "Number of troops must be a number" |> Error |> Err

        _ ->
            "You must move troops to your own country" |> Error |> Err


attemptToPlaceCapitol : Country.Id -> Player.Id -> Game -> Result Error Game
attemptToPlaceCapitol clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer _ ->
            "Error: Somehow you are placing a second capitol" |> Error |> Err

        OccupiedByOpponent _ ->
            "You must select an unoccuppied country" |> Error |> Err

        Unoccupied ->
            case Player.getPlayer currentPlayerId activeGame.players of
                Just currentPlayer ->
                    let
                        neutralTroopCount =
                            getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

                        updatedPlayer =
                            { currentPlayer
                                | countryTroopCounts =
                                    updateTroopCount clickedCountryId neutralTroopCount currentPlayer.countryTroopCounts
                                , capitolStatus = Player.Capitol clickedCountryId

                                -- , capitolStatus = Capitol clickedCountryId (Map.capitolDotsCoordinates clickedCountry.coordinates ViewHelpers.pixelsPerMapSquare)
                            }

                        updatedPlayers =
                            updatePlayer currentPlayerId updatedPlayer activeGame.players

                        -- TODO: This might be the only thing player id as int is used for
                        nextPlayerId =
                            case currentPlayerId of
                                Player.Id id ->
                                    Player.Id
                                        (remainderBy (Dict.size updatedPlayers)
                                            ((id |> String.toInt |> Maybe.withDefault -100) + 1)
                                            |> String.fromInt
                                        )

                        nextPlayerTurn =
                            case currentPlayerId of
                                Player.Id id ->
                                    if String.toInt id == Just (Dict.size updatedPlayers - 1) then
                                        PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement nextPlayerId

                                    else
                                        PlayerTurn.PlayerTurn PlayerTurn.CapitolPlacement nextPlayerId
                    in
                    Ok
                        { activeGame
                            | players = updatedPlayers
                            , neutralCountryTroops = destroyTroops clickedCountryId activeGame.neutralCountryTroops
                            , currentPlayerTurn = nextPlayerTurn
                        }

                Nothing ->
                    "Something bad happened" |> Error |> Err


attemptTroopPlacement : Country.Id -> Player.Id -> TroopCount.TroopCount -> Game -> Result Error Game
attemptTroopPlacement clickedCountryId currentPlayerId troopsToPlace activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer clickedCountryTroopCount ->
            let
                updatedGameResult =
                    activeGame |> updatePlayerTroopCountForCountry currentPlayerId clickedCountryId (TroopCount.addTroopCounts clickedCountryTroopCount troopsToPlace)
            in
            updatedGameResult
                |> Result.map
                    (\updatedGame ->
                        { updatedGame | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.AttackAnnexOrPort currentPlayerId }
                    )

        OccupiedByOpponent _ ->
            "You must put troops in your own country" |> Error |> Err

        Unoccupied ->
            "You must put troops in your own country" |> Error |> Err


attemptToBuildPort : Player.Id -> Country.Id -> Game -> Result Error Game
attemptToBuildPort currentPlayerId clickedCountryId activeGame =
    case getTroopCountForCountry clickedCountryId activeGame.players of
        Just _ ->
            buildPort currentPlayerId clickedCountryId activeGame

        Nothing ->
            "You can't build a port in a country you don't own" |> Error |> Err


attackResult : Country.Id -> Map.Map -> Player.Players -> PlayerTurn.PlayerTurn -> AttackResult
attackResult clickedCountryId gameMap players currentPlayerTurn =
    case findCountryOwner clickedCountryId players of
        Just opponentPlayerId ->
            let
                countryAttackers =
                    getAttackStrengthPerPlayer gameMap players clickedCountryId

                currentPlayerId =
                    PlayerTurn.getCurrentPlayer currentPlayerTurn

                currentPlayerIdString : String
                currentPlayerIdString =
                    case currentPlayerId of
                        Player.Id playerId ->
                            playerId

                attackStrength =
                    case Dict.get currentPlayerIdString countryAttackers of
                        Just attack ->
                            attack

                        Nothing ->
                            TroopCount.noTroops

                defenseStrength =
                    getCountryDefenseStrength gameMap players clickedCountryId

                remainingTroops =
                    getTroopCountForCountry clickedCountryId players
                        |> Maybe.withDefault TroopCount.noTroops
                        |> TroopCount.addTroopCounts defenseStrength
                        |> TroopCount.subtractTroopCounts attackStrength
            in
            if TroopCount.canAttack attackStrength defenseStrength then
                if TroopCount.hasTroops remainingTroops then
                    OpponentCountryLosesTroops remainingTroops

                else
                    case isCountryIdCapitol opponentPlayerId clickedCountryId players of
                        Just isCapitol ->
                            if isCapitol then
                                OpponentEliminated

                            else
                                CurrentPlayerAcquiresOpponentCountry

                        Nothing ->
                            AttackResultError "Error checking if capitol"

            else
                NotEnoughTroopsToAttack attackStrength defenseStrength

        Nothing ->
            AttackResultError "Error finding owner"


attemptSelectTroopMovementFromCountry : Country.Id -> Player.Id -> Game -> Result Error Game
attemptSelectTroopMovementFromCountry clickedCountryId currentPlayerId activeGame =
    case getCountryStatus clickedCountryId activeGame.players activeGame.currentPlayerTurn of
        OccupiedByCurrentPlayer troopCount ->
            if TroopCount.hasTroops troopCount then
                Ok
                    { activeGame
                        | currentPlayerTurn =
                            PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected clickedCountryId (TroopCount.toString troopCount)) currentPlayerId
                    }

            else
                "Select a country with troops" |> Error |> Err

        _ ->
            "You must move troops from your own country" |> Error |> Err


attemptToAnnexCountry : Player.Id -> Country.Id -> Game -> Result Error Game
attemptToAnnexCountry currentPlayerId clickedCountryId activeGame =
    if canAnnexCountry activeGame.map currentPlayerId activeGame.players clickedCountryId then
        let
            neutralTroopCount =
                getTroopCount clickedCountryId activeGame.neutralCountryTroops |> Maybe.withDefault TroopCount.noTroops

            updatedGameResult =
                updatePlayerTroopCountForCountry currentPlayerId clickedCountryId neutralTroopCount activeGame
        in
        updatedGameResult
            |> Result.map
                (\updatedGame ->
                    { updatedGame
                        | currentPlayerTurn = PlayerTurn.PlayerTurn PlayerTurn.TroopMovement currentPlayerId
                        , neutralCountryTroops = removeTroopCount clickedCountryId activeGame.neutralCountryTroops
                    }
                )

    else
        "You can't annex that country" |> Error |> Err


attemptToAttackCountry : Player.Id -> Country.Id -> Game -> Result Error Game
attemptToAttackCountry opponentPlayerId clickedCountryId activeGame =
    case attackResult clickedCountryId activeGame.map activeGame.players activeGame.currentPlayerTurn of
        OpponentCountryLosesTroops remainingTroops ->
            activeGame
                |> updatePlayerTroopCountForCountry opponentPlayerId clickedCountryId remainingTroops
                |> Result.map updateForSuccessfulAttack

        OpponentEliminated ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> Result.andThen (destroyPlayer opponentPlayerId)
                |> Result.map updateForSuccessfulAttack

        CurrentPlayerAcquiresOpponentCountry ->
            activeGame
                |> takeCountryFromOpponent clickedCountryId
                |> Result.map updateForSuccessfulAttack

        NotEnoughTroopsToAttack attackStrength defenseStrength ->
            ("Not enough to attack: attack strength = " ++ TroopCount.toString attackStrength ++ ", defense strength = " ++ TroopCount.toString defenseStrength)
                |> Error
                |> Err

        AttackResultError errorMessage ->
            errorMessage |> Error |> Err


filterCountriesOwnedBy : Player.Players -> Player.Id -> List Country.Id -> List Country.Id
filterCountriesOwnedBy players playerId countryIds =
    let
        countriesOwnedByPlayer : Set.Set String
        countriesOwnedByPlayer =
            case Player.getPlayer playerId players of
                Just player ->
                    player.countryTroopCounts |> Dict.keys |> Set.fromList

                Nothing ->
                    Set.empty
    in
    List.foldl
        (\(Country.Id countryId) result ->
            if Set.member countryId countriesOwnedByPlayer then
                Country.Id countryId :: result

            else
                result
        )
        []
        countryIds


buildPort : Player.Id -> Country.Id -> Game -> Result Error Game
buildPort playerId countryId activeGame =
    -- We already had to check that the player owned this country before so no need to do that here
    case Map.isCountryNeighboringWater countryId activeGame.map.countries of
        Just isNeighboringWater ->
            if isNeighboringWater then
                case getCountryHasPort countryId activeGame.players of
                    Just hasPort ->
                        if hasPort then
                            "This country already has a port" |> Error |> Err

                        else
                            let
                                updatedGameResult =
                                    activeGame
                                        |> updatePlayersWithPlayer playerId (Player.addPort countryId)

                                nextPlayerTurn =
                                    if playerHasMoreThanOneCountry activeGame.players playerId then
                                        PlayerTurn.PlayerTurn PlayerTurn.TroopMovement playerId

                                    else
                                        PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (playerId |> nextPlayerCheckForDeadPlayers activeGame.players)
                            in
                            updatedGameResult
                                |> Result.map (\updated -> { updated | currentPlayerTurn = nextPlayerTurn })

                    Nothing ->
                        "Error while building port" |> Error |> Err

            else
                "A country must be next to water to build a port" |> Error |> Err

        Nothing ->
            "Error checking if country borders water" |> Error |> Err


canAnnexCountry : Map.Map -> Player.Id -> Player.Players -> Country.Id -> Bool
canAnnexCountry gameMap playerId players countryIdToAnnex =
    -- We already know the country is unoccuppied from an earlier check so just make sure it is reachable from one of the current players countries
    case Player.getPlayer playerId players of
        Just player ->
            player.countryTroopCounts
                |> Dict.foldl
                    (\playerCountryId _ isReachable ->
                        isReachable || isCountryReachableFromOtherCountry (Country.Id playerCountryId) countryIdToAnnex gameMap.countries players
                    )
                    False

        Nothing ->
            -- This should never happen
            False


destroyPlayer : Player.Id -> Game -> Result Error Game
destroyPlayer playerId activeGame =
    -- Make this return result with error if dict lookup fails
    activeGame
        |> updatePlayersWithPlayer
            playerId
            (\player ->
                { player | capitolStatus = Player.NoCapitol, countryTroopCounts = Dict.empty }
            )


destroyTroops : Country.Id -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
destroyTroops (Country.Id countryId) neutralTroopCounts =
    Dict.remove countryId neutralTroopCounts


filterCountriesWithPort : Player.Players -> List Country.Id -> List Country.Id
filterCountriesWithPort players countries =
    countries
        |> List.filter
            (\countryId ->
                case getCountryHasPort countryId players of
                    Just hasPort ->
                        hasPort

                    _ ->
                        False
            )


findCountryOwnerAndTroopCount : Country.Id -> Player.Players -> Maybe ( Player.Id, TroopCount.TroopCount )
findCountryOwnerAndTroopCount (Country.Id countryId) players =
    players
        |> Dict.foldl
            (\playerId player result ->
                case result of
                    Just _ ->
                        result

                    Nothing ->
                        Dict.get countryId player.countryTroopCounts
                            |> Maybe.map (\troopCount -> ( Player.Id playerId, troopCount ))
            )
            Nothing


getCountryCanBeClicked : PlayerTurn.PlayerTurn -> Player.Players -> Map.Map -> Country.Id -> Bool
getCountryCanBeClicked currentPlayerTurn players gameMap countryId =
    case currentPlayerTurn of
        PlayerTurn.PlayerTurn playerTurnStatus currentPlayerId ->
            case playerTurnStatus of
                PlayerTurn.CapitolPlacement ->
                    case findCountryOwner countryId players of
                        Just _ ->
                            False

                        Nothing ->
                            True

                PlayerTurn.TroopPlacement ->
                    case findCountryOwner countryId players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                PlayerTurn.AttackAnnexOrPort ->
                    let
                        canAttack =
                            case attackResult countryId gameMap players currentPlayerTurn of
                                NotEnoughTroopsToAttack _ _ ->
                                    False

                                AttackResultError _ ->
                                    False

                                _ ->
                                    True

                        canAnnex =
                            canAnnexCountry gameMap currentPlayerId players countryId

                        canBuildPort =
                            False
                    in
                    canAttack || canAnnex || canBuildPort

                PlayerTurn.TroopMovement ->
                    case findCountryOwner countryId players of
                        Just countryOwnerId ->
                            countryOwnerId == currentPlayerId

                        Nothing ->
                            False

                PlayerTurn.TroopMovementFromSelected fromCountryId _ ->
                    if isCountryReachableFromOtherCountry fromCountryId countryId gameMap.countries players then
                        case Player.getPlayer (PlayerTurn.getCurrentPlayer currentPlayerTurn) players of
                            Just currentPlayer ->
                                case getTroopCount countryId currentPlayer.countryTroopCounts of
                                    Just _ ->
                                        True

                                    Nothing ->
                                        False

                            Nothing ->
                                False

                    else
                        False

                _ ->
                    False


getCountryStatus : Country.Id -> Player.Players -> PlayerTurn.PlayerTurn -> CountryStatus
getCountryStatus countryId players currentPlayerTurn =
    case Player.getPlayer (PlayerTurn.getCurrentPlayer currentPlayerTurn) players of
        Just currentPlayer ->
            case getTroopCount countryId currentPlayer.countryTroopCounts of
                Just troopCount ->
                    OccupiedByCurrentPlayer troopCount

                Nothing ->
                    case
                        players
                            |> Dict.foldl
                                (\playerId player result ->
                                    case result of
                                        Just _ ->
                                            result

                                        Nothing ->
                                            getTroopCount countryId player.countryTroopCounts
                                                |> Maybe.map (\_ -> OccupiedByOpponent (Player.Id playerId))
                                )
                                Nothing
                    of
                        Just occupiedByOppenent ->
                            occupiedByOppenent

                        Nothing ->
                            Unoccupied

        Nothing ->
            Unoccupied


getDefenseThroughWater : Map.Map -> Player.Players -> Country.Id -> Dict.Dict String TroopCount.TroopCount
getDefenseThroughWater gameMap players countryId =
    case findCountryOwner countryId players of
        Just playerId ->
            let
                countriesReachableThroughWater : List Country.Id
                countriesReachableThroughWater =
                    Map.getCountriesThatCanReachCountryThroughWater gameMap.countries gameMap.bodiesOfWater countryId

                defenderCountriesNeighboringWater : List Country.Id
                defenderCountriesNeighboringWater =
                    countriesReachableThroughWater
                        |> filterCountriesOwnedBy players playerId

                defenderCountriesNeighoboringWaterWithPort : List Country.Id
                defenderCountriesNeighoboringWaterWithPort =
                    defenderCountriesNeighboringWater
                        |> filterCountriesWithPort players
                        |> List.filter (\country -> country /= countryId)
            in
            defenderCountriesNeighoboringWaterWithPort
                |> List.foldl
                    (\(Country.Id countryWithPortId) result ->
                        case getTroopCountForCountry (Country.Id countryWithPortId) players of
                            Just troopCount ->
                                result |> Dict.insert countryWithPortId troopCount

                            Nothing ->
                                result
                    )
                    Dict.empty

        Nothing ->
            Dict.empty


getIsBeingMovedFrom : PlayerTurn.PlayerTurn -> Country.Id -> Bool
getIsBeingMovedFrom currentPlayerTurn countryId =
    case currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected fromCountryId _) _ ->
            fromCountryId == countryId

        _ ->
            False


isCountryOwnedByPlayer : Player.Id -> Country.Id -> Player.Players -> Bool
isCountryOwnedByPlayer playerId countryId players =
    case Player.getPlayer playerId players of
        Just currentPlayer ->
            case getTroopCount countryId currentPlayer.countryTroopCounts of
                Just _ ->
                    True

                Nothing ->
                    False

        Nothing ->
            False


isCountryReachableFromOtherCountry : Country.Id -> Country.Id -> Dict.Dict String Country.Country -> Player.Players -> Bool
isCountryReachableFromOtherCountry fromCountryId toCountryId countries players =
    case Country.getCountry fromCountryId countries of
        Just fromCountry ->
            case toCountryId of
                Country.Id toId ->
                    if Set.member toId fromCountry.neighboringCountries then
                        True

                    else if toCountryId /= fromCountryId then
                        case ( getCountryHasPort fromCountryId players, Country.getCountry toCountryId countries ) of
                            ( Just hasPort, Just toCountry ) ->
                                if hasPort then
                                    Set.size (Set.intersect fromCountry.neighboringBodiesOfWater toCountry.neighboringBodiesOfWater) > 0

                                else
                                    False

                            _ ->
                                -- shouldn't happen
                                False

                    else
                        False

        Nothing ->
            False



-- TODO


nextPlayer : Player.Players -> Player.Id -> Player.Id
nextPlayer players (Player.Id currentPlayerId) =
    remainderBy (Dict.size players)
        ((currentPlayerId
            |> String.toInt
            |> Maybe.withDefault -100
         )
            + 1
        )
        |> String.fromInt
        |> Player.Id


nextPlayerCheckForDeadPlayers : Player.Players -> Player.Id -> Player.Id
nextPlayerCheckForDeadPlayers players currentPlayerId =
    -- This doesn't work during capitol placement because nobody will have a capitol except player 1 after player 1 places their capitol
    let
        nextPlayerId =
            currentPlayerId |> nextPlayer players
    in
    case Player.getPlayer nextPlayerId players of
        Just newCurrentPlayer ->
            case newCurrentPlayer |> .capitolStatus of
                Player.Capitol _ ->
                    nextPlayerId

                Player.NoCapitol ->
                    nextPlayerId |> nextPlayerCheckForDeadPlayers players

        Nothing ->
            currentPlayerId


neutralCountryColor : Colors.Color
neutralCountryColor =
    Colors.gray


playerHasMoreThanOneCountry : Player.Players -> Player.Id -> Bool
playerHasMoreThanOneCountry players playerId =
    Player.getPlayer playerId players
        |> Maybe.map (\player -> Dict.size player.countryTroopCounts > 1)
        |> Maybe.withDefault False


playerTurnToPlayerId : PlayerTurn.PlayerTurn -> Player.Id
playerTurnToPlayerId (PlayerTurn.PlayerTurn _ playerId) =
    playerId


removePlayerCountry : Country.Id -> Game -> Result Error Game
removePlayerCountry (Country.Id countryId) activeGame =
    -- Make this return result with error if dict lookup fails
    case findCountryOwner (Country.Id countryId) activeGame.players of
        Just playerId ->
            activeGame
                |> updatePlayersWithPlayer playerId
                    (\player ->
                        { player
                            | countryTroopCounts = player.countryTroopCounts |> Dict.remove countryId
                        }
                    )

        Nothing ->
            "Error finding country owner" |> Error |> Err


removeTroopCount : Country.Id -> Dict.Dict String TroopCount.TroopCount -> Dict.Dict String TroopCount.TroopCount
removeTroopCount (Country.Id countryId) troopCounts =
    Dict.remove countryId troopCounts


takeCountryFromOpponent : Country.Id -> Game -> Result Error Game
takeCountryFromOpponent countryId activeGame =
    activeGame
        |> removePlayerCountry countryId
        |> Result.andThen
            (\updatedGame ->
                updatedGame |> updatePlayerTroopCountForCountry (PlayerTurn.getCurrentPlayer activeGame.currentPlayerTurn) countryId TroopCount.noTroops
            )


updateForSuccessfulAttack : Game -> Game
updateForSuccessfulAttack activeGame =
    let
        currentPlayerId =
            activeGame.currentPlayerTurn |> playerTurnToPlayerId

        nextPlayerTurn =
            let
                capitolsRemaining =
                    activeGame.players
                        |> Dict.values
                        |> List.foldl
                            (\player capitols ->
                                case player.capitolStatus of
                                    Player.Capitol capitolId ->
                                        capitolId :: capitols

                                    Player.NoCapitol ->
                                        capitols
                            )
                            []
            in
            if List.length capitolsRemaining == 1 then
                PlayerTurn.PlayerTurn PlayerTurn.GameOver currentPlayerId

            else if playerHasMoreThanOneCountry activeGame.players currentPlayerId then
                PlayerTurn.PlayerTurn PlayerTurn.TroopMovement currentPlayerId

            else
                PlayerTurn.PlayerTurn PlayerTurn.TroopPlacement (currentPlayerId |> nextPlayerCheckForDeadPlayers activeGame.players)
    in
    { activeGame
        | currentPlayerTurn = nextPlayerTurn
    }


updatePlayer : Player.Id -> Player.Player -> Player.Players -> Player.Players
updatePlayer (Player.Id playerId) player players =
    Dict.insert playerId player players


updatePlayerTroopCountForCountry : Player.Id -> Country.Id -> TroopCount.TroopCount -> Game -> Result Error Game
updatePlayerTroopCountForCountry playerId countryId troops activeGame =
    -- Make this return result with error if dict lookup fails
    activeGame
        |> updatePlayersWithPlayer playerId
            (\player ->
                { player
                    | countryTroopCounts =
                        player.countryTroopCounts
                            |> updateTroopCount countryId troops
                }
            )


updatePlayersWithPlayer : Player.Id -> (Player.Player -> Player.Player) -> Game -> Result Error Game
updatePlayersWithPlayer playerId toUpdatedPlayer activeGame =
    case Player.getPlayer playerId activeGame.players of
        Just player ->
            Ok
                { activeGame
                    | players =
                        activeGame.players
                            |> updatePlayer playerId (toUpdatedPlayer player)
                }

        Nothing ->
            "some error" |> Error |> Err


updateTroopCount :
    Country.Id
    -> TroopCount.TroopCount
    -> Dict.Dict String TroopCount.TroopCount
    -> Dict.Dict String TroopCount.TroopCount
updateTroopCount (Country.Id countryId) troopCount troopCounts =
    Dict.insert countryId troopCount troopCounts


type Error
    = Error String
