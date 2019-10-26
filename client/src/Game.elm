module Game exposing
    ( AttackResult(..)
    , CountryBorderHelperOutlineStatus(..)
    , CountryStatus(..)
    , Game
    , Id(..)
    , Model
    , Msg(..)
    , attackResult
    , canAnnexCountry
    , filterCountriesWithPort
    , findCountryOwner
    , getCountryHasPort
    , getCountryStatus
    , getTroopCount
    , getTroopCountForCountry
    , idToString
    , isCountryAttacking
    , isCountryDefending
    , isCountryReachableFromOtherCountry
    , view
    , viewGameWrapMessage
    )

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
import Html
import Html.Attributes
import Map
import Player
import PlayerTurn
import Set
import TroopCount
import ViewHelpers


idToString : Id -> String
idToString (Id id) =
    id


type alias Model =
    { activeGame : Game
    , showAvailableMoves : Bool
    , error : Maybe String
    , countryBorderHelperOutlineStatus : CountryBorderHelperOutlineStatus
    }


type alias Game =
    { id : Id
    , currentPlayerTurn : PlayerTurn.PlayerTurn
    , map : Map.Map
    , players : Player.Players
    , currentUserPlayerId : Player.Id
    , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
    }


type Id
    = Id String


type CountryBorderHelperOutlineStatus
    = CountryBorderHelperOutlineWaitingForDelay Country.Id
    | CountryBorderHelperOutlineInactive
    | CountryBorderHelperOutlineActive Country.Id


type Msg
    = CountryMouseUp Country.Id
    | CountryMouseDown Country.Id
    | CountryMouseOut Country.Id
    | MouseUp
    | Pass
    | UpdateNumberOfTroopsToMove String
    | CancelMovingTroops
    | ShowCountryBorderHelper
    | ShowAvailableMovesCheckboxToggled Bool



---- VIEW ----


viewGameWrapMessage : Model -> { width : Int, height : Int } -> (Msg -> msg) -> { title : String, content : Html.Html msg }
viewGameWrapMessage model windowSize toMsg =
    let
        { title, content } =
            viewGame model windowSize

        updatedContent =
            content |> Html.map toMsg
    in
    { title = title, content = updatedContent }


viewGame : Model -> { width : Int, height : Int } -> { title : String, content : Html.Html Msg }
viewGame gameLoaded windowSize =
    { content =
        let
            device =
                Element.classifyDevice windowSize
        in
        case Element.classifyDevice windowSize |> .class of
            Element.Phone ->
                viewPlayingGameMobile gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device

            _ ->
                viewPlayingGameDesktop gameLoaded.activeGame gameLoaded.showAvailableMoves gameLoaded.countryBorderHelperOutlineStatus gameLoaded.error device
    , title = "Fracas"
    }


view : Model -> { width : Int, height : Int } -> (Msg -> msg) -> { title : String, content : Html.Html msg }
view model windowSize toMsg =
    let
        { title, content } =
            viewGame model windowSize
    in
    { title = title, content = content |> Html.map toMsg }


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
        [ viewPassButtonIfNecessary activeGame.currentPlayerTurn (PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn activeGame.currentUserPlayerId)
        , viewPlayerCountryAndTroopCountsMobile activeGame.currentPlayerTurn activeGame.players
        , viewConfigureTroopCountIfNecessary activeGame.currentPlayerTurn (PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn activeGame.currentUserPlayerId)
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
        [ viewPassButtonIfNecessary activeGame.currentPlayerTurn (PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn activeGame.currentUserPlayerId)
        , viewPlayerCountryAndTroopCounts activeGame.currentPlayerTurn activeGame.players
        , viewConfigureTroopCountIfNecessary activeGame.currentPlayerTurn (PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn activeGame.currentUserPlayerId)
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


viewPassButtonIfNecessary : PlayerTurn.PlayerTurn -> Bool -> Element.Element Msg
viewPassButtonIfNecessary currentPlayerTurn isCurrentUserPlayerTurn =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 50)
        ]
        (if isCurrentUserPlayerTurn && PlayerTurn.canCurrentPlayerPass currentPlayerTurn then
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


viewConfigureTroopCountIfNecessary : PlayerTurn.PlayerTurn -> Bool -> Element.Element Msg
viewConfigureTroopCountIfNecessary currentPlayerTurn isCurrentUsersTurn =
    Element.el
        [ Element.width Element.fill
        ]
        (if isCurrentUsersTurn then
            case currentPlayerTurn |> PlayerTurn.troopsToMove of
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

         else
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


neutralCountryColor : Colors.Color
neutralCountryColor =
    Colors.gray


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


getTroopCount : Country.Id -> Dict.Dict String TroopCount.TroopCount -> Maybe TroopCount.TroopCount
getTroopCount (Country.Id countryId) troopCounts =
    Dict.get countryId troopCounts


getIsBeingMovedFrom : PlayerTurn.PlayerTurn -> Country.Id -> Bool
getIsBeingMovedFrom currentPlayerTurn countryId =
    case currentPlayerTurn of
        PlayerTurn.PlayerTurn (PlayerTurn.TroopMovementFromSelected fromCountryId _) _ ->
            fromCountryId == countryId

        _ ->
            False


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


findCountryOwner : Country.Id -> Player.Players -> Maybe Player.Id
findCountryOwner countryId players =
    findCountryOwnerAndTroopCount countryId players
        |> Maybe.map Tuple.first


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


getTroopCountForCountry : Country.Id -> Player.Players -> Maybe TroopCount.TroopCount
getTroopCountForCountry countryId players =
    findCountryOwner countryId players
        |> Maybe.andThen (\playerId -> Player.getPlayer playerId players)
        |> Maybe.andThen (\player -> getTroopCount countryId player.countryTroopCounts)


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
