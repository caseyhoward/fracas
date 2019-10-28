module Game.InfoPanel exposing
    ( AlivePlayerTroopCount
    , Attacker
    , CountryInfo
    , CurrentPlayerTurnCountryAndTroopCount
    , DeadPlayerTroopCount
    , Model
    , Msg(..)
    , PlayerCountryAndTroopCount(..)
    , PlayerCountryAndTroopCounts
    , TroopMovement(..)
    , TurnStage(..)
    , viewInfoPanelDesktop
    , viewInfoPanelPhone
    )

import Colors
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import TroopCount
import ViewHelpers


type alias Model =
    { canPass : Bool
    , troopMovement : TroopMovement
    , showAvailableMoves : Bool
    , troopCounts : PlayerCountryAndTroopCounts
    , countryInfo : Maybe CountryInfo
    }


type alias Attacker =
    { troopCount : TroopCount.TroopCount
    , name : String
    , color : Colors.Color
    }


type alias CountryInfo =
    { playerColor : Colors.Color
    , defenseStrength : TroopCount.TroopCount
    , attackers : List Attacker
    }


type alias CurrentPlayerTurnCountryAndTroopCount =
    { playerTroopCounts : AlivePlayerTroopCount
    , turnStage : TurnStage
    }


type PlayerCountryAndTroopCount
    = DeadPlayerTroopCountCase DeadPlayerTroopCount
    | AlivePlayerTroopCountCase AlivePlayerTroopCount


type alias PlayerCountryAndTroopCounts =
    { playerTroopCountsBefore : List PlayerCountryAndTroopCount
    , currentPlayerTurnTroopCounts : CurrentPlayerTurnCountryAndTroopCount
    , playerTroopCountsAfter : List PlayerCountryAndTroopCount
    }


type alias AlivePlayerTroopCount =
    { playerColor : Colors.Color
    , playerName : String
    , troopCount : TroopCount.TroopCount
    , isCurrentUser : Bool
    , countryCount : Int
    }


type alias DeadPlayerTroopCount =
    { playerColor : Colors.Color
    , playerName : String
    , isCurrentUser : Bool
    }


type TroopMovement
    = MovingTroops String
    | NotMovingTroops


type TurnStage
    = CapitolPlacement
    | TroopPlacement
    | AttackPassOrBuildPort
    | TroopMovement
    | WaitingForTurn



--- MESSAGE ----


type Msg
    = ShowAvailableMovesCheckboxToggled Bool
    | Pass
    | CancelTroopMovement
    | TroopCountChanged String



---- EXPOSED ----


viewInfoPanelPhone : Model -> Element.Element Msg
viewInfoPanelPhone model =
    Element.column
        (Element.width Element.fill :: infoPanelAttributes)
        [ if model.canPass then
            passButton

          else
            Element.el
                [ Element.height (Element.px 50)
                ]
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


viewInfoPanelDesktop : Model -> Element.Element Msg
viewInfoPanelDesktop model =
    Element.column
        (Element.width (Element.px 300) :: infoPanelAttributes)
        [ Element.el [ Element.height (Element.px 120), Element.width Element.fill ]
            (case model.troopMovement of
                MovingTroops troopCountString ->
                    viewConfigureTroopCount troopCountString

                NotMovingTroops ->
                    if model.canPass then
                        passButton

                    else
                        Element.none
            )
        , viewPlayerCountryAndTroopCounts model.troopCounts
        , case model.countryInfo of
            Just countryInfo ->
                viewCountryInfo countryInfo

            Nothing ->
                Element.none
        , viewShowAvailableMoves model.showAvailableMoves
        ]



---- LOCAL ----


currentPlayerTurnTroopCountView : CurrentPlayerTurnCountryAndTroopCount -> Element.Element Msg
currentPlayerTurnTroopCountView { playerTroopCounts, turnStage } =
    viewPlayerTroopCount
        playerTroopCounts.playerName
        Colors.black
        playerTroopCounts.playerColor
        playerTroopCounts.countryCount
        playerTroopCounts.troopCount
        turnStage
        playerTroopCounts.isCurrentUser


passButton : Element.Element Msg
passButton =
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
    , Element.spacing 40
    , Element.alignTop
    , Element.Background.color (Colors.darkCharcoal |> Colors.toElementColor)
    ]


playerTroopCountView : PlayerCountryAndTroopCount -> Element.Element Msg
playerTroopCountView viewModel =
    case viewModel of
        AlivePlayerTroopCountCase playerTroopCounts ->
            viewPlayerTroopCount
                playerTroopCounts.playerName
                Colors.black
                playerTroopCounts.playerColor
                playerTroopCounts.countryCount
                playerTroopCounts.troopCount
                WaitingForTurn
                playerTroopCounts.isCurrentUser

        DeadPlayerTroopCountCase deadPlayerTroopCount ->
            viewPlayerTroopCount
                deadPlayerTroopCount.playerName
                Colors.gray
                deadPlayerTroopCount.playerColor
                0
                TroopCount.noTroops
                WaitingForTurn
                deadPlayerTroopCount.isCurrentUser


turnIndicatorWidth : Element.Attribute msg
turnIndicatorWidth =
    Element.width (Element.px 26)


turnStageView : TurnStage -> Element.Element msg
turnStageView turnStage =
    let
        activeColor =
            Colors.black

        inactiveColor =
            Colors.darkGray
    in
    case turnStage of
        CapitolPlacement ->
            Element.el
                [ Element.height Element.fill
                , turnIndicatorWidth
                , Element.Background.color (Colors.white |> Colors.toElementColor)
                , Element.Border.rounded 2
                , Element.padding 2
                ]
                (ViewHelpers.fontawesomeIcon "fab" "fort-awesome" "xs")

        TroopPlacement ->
            viewTurnIndicator activeColor inactiveColor inactiveColor

        AttackPassOrBuildPort ->
            viewTurnIndicator inactiveColor activeColor inactiveColor

        TroopMovement ->
            viewTurnIndicator inactiveColor inactiveColor activeColor

        WaitingForTurn ->
            Element.el
                [ Element.height Element.fill
                , turnIndicatorWidth
                , Element.Border.rounded 2
                , Element.padding 2
                ]
                Element.none


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
            , label =
                Element.Input.labelAbove
                    (ViewHelpers.defaultLabelAttributes ++ [ Element.alignLeft, Element.Font.color (Colors.white |> Colors.toElementColor) ])
                    (Element.text "Number of troops to move")
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
        [ Element.width Element.fill
        , Element.spacing 5
        , Element.padding 5
        , Element.Border.rounded 5
        , Element.Background.color (Colors.white |> Colors.toElementColor)
        ]
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
        , if List.length countryInfo.attackers > 0 then
            Element.column
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

          else
            Element.el
                [ Element.Font.size 14
                ]
                (Element.text "No attackers")
        ]


viewPlayerTroopCount :
    String
    -> Colors.Color
    -> Colors.Color
    -> Int
    -> TroopCount.TroopCount
    -> TurnStage
    -> Bool
    -> Element.Element Msg
viewPlayerTroopCount playerName fontColor playerColor countryCount troopCount turnStage isCurrentUserTurn =
    Element.row
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
                (Element.row [ Element.centerX, Element.padding 2, Element.spacing 10 ]
                    [ Element.el [] (Element.text <| playerName)
                    , if isCurrentUserTurn then
                        Element.el [] (ViewHelpers.fontawesomeIcon "fas" "user" "xs")

                      else
                        Element.none
                    ]
                )
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


viewTurnIndicator : Colors.Color -> Colors.Color -> Colors.Color -> Element.Element msg
viewTurnIndicator troopPlacementColor attackColor troopMovementColor =
    Element.column
        [ Element.height Element.fill
        , Element.Background.color (Colors.white |> Colors.toElementColor)
        , turnIndicatorWidth
        , Element.Border.rounded 2
        , Element.padding 3
        , Element.spacing 8
        ]
        [ Element.el [ Element.Font.color (troopPlacementColor |> Colors.toElementColor) ] (ViewHelpers.fontawesomeIcon "fas" "parachute-box" "xs")
        , Element.el [ Element.Font.color (attackColor |> Colors.toElementColor) ] (ViewHelpers.fontawesomeIcon "fas" "fighter-jet" "xs")
        , Element.el [ Element.Font.color (troopMovementColor |> Colors.toElementColor) ] (ViewHelpers.fontawesomeIcon "fas" "shuttle-van" "xs")
        ]
