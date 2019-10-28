module Game.InfoPanel exposing
    ( CurrentPlayerTurnCountryAndTroopCount
    , Model
    , Msg(..)
    , PlayerCountryAndTroopCount(..)
    , PlayerCountryAndTroopCounts
    , TroopCounts
    , TroopMovement(..)
    , TurnStage(..)
    , viewInfoPanelDesktop
    , viewInfoPanelPhone
    )

import Colors
import Country
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Player
import TroopCount
import ViewHelpers



-- type alias Game =
--     { id : Id
--     , currentPlayerTurn : PlayerTurn.PlayerTurn
--     , map : Map.Map
--     , players : Player.Players
--     , currentUserPlayerId : Player.Id
--     , neutralCountryTroops : Dict.Dict String TroopCount.TroopCount
--     }


playerAndTroopCountBorderAttributes : Colors.Color -> List (Element.Attribute Msg)
playerAndTroopCountBorderAttributes borderColor =
    [ Element.Border.solid
    , Element.width Element.fill
    , Element.Border.width 2
    , Element.Border.rounded 10
    , Element.Border.color (borderColor |> Colors.toElementColor)
    , Element.padding 4
    ]



-- (attackerStrengthPerPlayer
--     |> Dict.toList
--     |> List.filter
--         (\( playerId, troopCount ) ->
--             Player.Id playerId /= countyOwnerPlayerId && troopCount /= TroopCount.noTroops
--         )
--     |> List.map
--         (\( playerId, troopCount ) ->
--             case Player.getPlayer (Player.Id playerId) activeGame.players of
--                 Just player ->
--
--                 Nothing ->
--                     Element.none
--         )
-- )


type TroopMovement
    = MovingTroops String
    | NotMovingTroops


type alias Model =
    { canPass : Bool
    , troopMovement : TroopMovement
    , showAvailableMoves : Bool
    , troopCounts : PlayerCountryAndTroopCounts
    , countryInfo : Maybe CountryInfo
    }


type alias CountryInfo =
    { playerColor : Colors.Color
    , defenseStrength : TroopCount.TroopCount
    , attackers : List Attacker
    }


type alias Attacker =
    { troopCount : TroopCount.TroopCount
    , name : String
    , color : Colors.Color
    }


type alias PlayerCountryAndTroopCounts =
    { playerTroopCountsBefore : List PlayerCountryAndTroopCount
    , currentPlayerTurnTroopCounts : CurrentPlayerTurnCountryAndTroopCount
    , playerTroopCountsAfter : List PlayerCountryAndTroopCount
    }


type PlayerCountryAndTroopCount
    = DeadPlayerTroopCount String Colors.Color
    | AlivePlayerTroopCount TroopCounts


type alias TroopCounts =
    { playerColor : Colors.Color
    , playerName : String
    , troopCount : TroopCount.TroopCount
    , countryCount : Int
    }


type alias CurrentPlayerTurnCountryAndTroopCount =
    { playerTroopCounts : TroopCounts
    , turnStage : TurnStage
    }


type TurnStage
    = CapitolPlacement
    | TroopPlacement
    | AttackPassOrBuildPort
    | TroopMovement
    | WaitingForTurn


type Msg
    = ShowAvailableMovesCheckboxToggled Bool
    | Pass
    | CancelTroopMovement
    | TroopCountChanged String


viewInfoPanelPhone : Model -> Element.Element Msg
viewInfoPanelPhone model =
    Debug.todo ""



-- Element.column
--     (Element.width Element.fill :: infoPanelAttributes)
--     [ viewPassButtonIfNecessary activeGame.currentPlayerTurn (PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn activeGame.currentUserPlayerId)
--     , viewPlayerCountryAndTroopCountsMobile activeGame.currentPlayerTurn activeGame.players
--     , viewConfigureTroopCountIfNecessary activeGame.currentPlayerTurn (PlayerTurn.isPlayerTurn activeGame.currentPlayerTurn activeGame.currentUserPlayerId)
--     , viewCountryInfo activeGame countryBorderHelperOutlineStatus
--     , viewShowAvailableMoves showAvailableMoves
--     ]


viewInfoPanelDesktop : Model -> Element.Element Msg
viewInfoPanelDesktop model =
    Element.column
        (Element.width (Element.px 200) :: infoPanelAttributes)
        [ if model.canPass then
            viewPassButton

          else
            Element.none
        , case model.troopMovement of
            MovingTroops troopCountString ->
                viewConfigureTroopCount troopCountString

            NotMovingTroops ->
                Element.none
        , viewPlayerCountryAndTroopCounts model.troopCounts
        , case model.countryInfo of
            Just countryInfo ->
                viewCountryInfo countryInfo

            Nothing ->
                Element.none
        , viewShowAvailableMoves model.showAvailableMoves
        ]


viewPassButton : Element.Element Msg
viewPassButton =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 50)
        ]
        (Element.Input.button
            (ViewHelpers.defaultButtonAttributes
                ++ [ Element.width (Element.px 120)
                   , Element.centerX
                   , 40 |> Element.px |> Element.height
                   , Element.Background.color (Element.rgb255 100 200 100)
                   ]
            )
            { onPress = Just Pass, label = ViewHelpers.centerText "Pass" }
        )


countryAttackersView : List Attacker -> Element.Element Msg
countryAttackersView attackers =
    let
        attackerView attacker =
            Element.row
                [ Element.width Element.fill
                , Element.Font.size 14
                , Element.padding 3
                , Element.Background.color (attacker.color |> ViewHelpers.colorToElementColor)
                ]
                [ Element.el [] (Element.text attacker.name)
                , Element.el
                    [ Element.alignRight ]
                    (attacker.troopCount |> TroopCount.toString |> Element.text)
                ]
    in
    Element.column
        [ Element.width Element.fill, Element.spacing 3 ]
        (attackers |> List.map attackerView)


infoPanelAttributes : List (Element.Attribute Msg)
infoPanelAttributes =
    [ Element.height Element.fill
    , Element.padding 20
    , Element.spacing 20
    , Element.alignTop
    , Element.Background.color (Colors.darkCharcoal |> Colors.toElementColor)
    ]


playerAndTroopCountBorderColor : Element.Color
playerAndTroopCountBorderColor =
    Colors.white |> ViewHelpers.colorToElementColor


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


viewConfigureTroopCount : String -> Element.Element Msg
viewConfigureTroopCount numberOfTroopsToMove =
    Element.column
        [ Element.width Element.fill
        , Element.padding 10
        ]
        [ Element.Input.text
            (ViewHelpers.defaultTextInputAttributes ++ [ Element.alignLeft ])
            { onChange = TroopCountChanged
            , placeholder = Nothing
            , label = Element.Input.labelAbove (ViewHelpers.defaultLabelAttributes ++ [ Element.alignLeft ]) (Element.text "Number of troops to move")
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
            { onPress = Just CancelTroopMovement, label = ViewHelpers.centerText "Cancel" }
        ]


viewCountryInfo : CountryInfo -> Element.Element Msg
viewCountryInfo countryInfo =
    Element.column
        [ Element.width Element.fill, Element.spacing 5 ]
        [ Element.el
            [ Element.Background.color (countryInfo.playerColor |> ViewHelpers.colorToElementColor)
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
                (countryInfo.defenseStrength |> TroopCount.toString |> Element.text)
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
            , countryAttackersView countryInfo.attackers
            ]
        ]


viewPlayerTroopCount :
    String
    -> Colors.Color
    -> Colors.Color
    -> Int
    -> TroopCount.TroopCount
    -> TurnStage
    -> Element.Element Msg
viewPlayerTroopCount playerName fontColor playerColor countryCount troopCount turnStage =
    Element.el
        (playerAndTroopCountBorderAttributes Colors.black)
        (Element.row
            [ Element.width Element.fill, Element.spacing 3 ]
            [ Element.column
                [ Element.spacing 1
                , Element.width Element.fill
                , Element.Background.color (Colors.white |> Colors.toElementColor)
                , Element.Border.color (Colors.white |> Colors.toElementColor)
                , Element.Border.rounded 5
                , Element.paddingXY 2 5
                , Element.Font.color (fontColor |> Colors.toElementColor)
                , Element.Background.color (playerColor |> ViewHelpers.colorToElementColor)
                ]
                [ Element.el
                    [ Element.width Element.fill
                    , Element.Font.size 14
                    , Element.Font.bold
                    , Element.centerX
                    ]
                    (Element.el [ Element.centerX, Element.padding 2 ] (Element.text <| playerName))
                , Element.column
                    [ Element.Background.color (Colors.lightGray |> ViewHelpers.colorToElementColor)
                    , Element.width Element.fill
                    , Element.Border.rounded 2
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
                            (Element.text (String.fromInt countryCount))
                        ]
                    , Element.row [ Element.spacing 20, Element.Font.size 16 ]
                        [ Element.el
                            [ Element.width (Element.px 80)
                            , Element.padding 3
                            ]
                            (Element.el [ Element.alignRight ] (Element.text "Troops"))
                        , Element.el
                            []
                            (Element.text (TroopCount.toString troopCount))
                        ]
                    ]
                ]
            , turnStageView turnStage
            ]
        )


turnStageView : TurnStage -> Element.Element msg
turnStageView turnStage =
    case turnStage of
        CapitolPlacement ->
            Element.el
                [ Element.height Element.fill
                , Element.width (Element.px 16)
                , Element.Background.color (Colors.white |> Colors.toElementColor)
                , Element.Border.rounded 2
                , Element.padding 2
                ]
                (ViewHelpers.fontawesomeIcon "fort-awesome")

        _ ->
            Element.none


viewPlayerCountryAndTroopCounts : PlayerCountryAndTroopCounts -> Element.Element Msg
viewPlayerCountryAndTroopCounts viewModel =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        ]
        (List.concat
            [ viewModel
                |> .playerTroopCountsBefore
                |> List.map playerTroopCountView
            , viewModel.currentPlayerTurnTroopCounts
                |> currentPlayerTurnTroopCountView
                |> List.singleton
            , viewModel.playerTroopCountsAfter
                |> List.map playerTroopCountView
            ]
        )


playerTroopCountView : PlayerCountryAndTroopCount -> Element.Element Msg
playerTroopCountView viewModel =
    case viewModel of
        AlivePlayerTroopCount playerTroopCounts ->
            viewPlayerTroopCount
                playerTroopCounts.playerName
                Colors.black
                playerTroopCounts.playerColor
                playerTroopCounts.countryCount
                playerTroopCounts.troopCount
                WaitingForTurn

        DeadPlayerTroopCount playerName playerColor ->
            viewPlayerTroopCount
                playerName
                Colors.gray
                playerColor
                0
                TroopCount.noTroops
                WaitingForTurn


currentPlayerTurnTroopCountView : CurrentPlayerTurnCountryAndTroopCount -> Element.Element Msg
currentPlayerTurnTroopCountView { playerTroopCounts, turnStage } =
    viewPlayerTroopCount playerTroopCounts.playerName Colors.black playerTroopCounts.playerColor playerTroopCounts.countryCount playerTroopCounts.troopCount turnStage
