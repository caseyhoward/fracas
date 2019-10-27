module NewGame exposing
    ( addPlayerButton
    , colorButton
    , configurationSectionAttributes
    , mapConfiguration
    , mapConfigurationFields
    , mapView
    , removePlayerButton
    , removePlayerButtonWidth
    , startGameButton
    , title
    )

import Colors
import Country
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Lazy
import Graphql.Http
import Map
import Maps.FracasTitle
import Player
import RemoteData
import ViewHelpers


addPlayerButton : msg -> Element.Element msg
addPlayerButton message =
    Element.Input.button
        (ViewHelpers.defaultButtonAttributes
            ++ [ Element.Background.color (Colors.blue |> Colors.toElementColor)
               , Element.Font.color (Colors.white |> Colors.toElementColor)
               ]
        )
        { onPress = Just message, label = ViewHelpers.centerText "Add Player" }


colorButton : Colors.Color -> msg -> Element.Element msg
colorButton color message =
    Element.Input.button
        (ViewHelpers.defaultButtonAttributes
            ++ [ Element.Background.color (color |> Colors.toElementColor)
               , Element.height Element.fill
               , Element.width (Element.px 50)
               ]
        )
        { onPress = Just message, label = Element.text "" }


configurationSectionAttributes : List (Element.Attribute msg)
configurationSectionAttributes =
    [ Element.Background.color (Colors.gray |> Colors.toElementColor)
    , Element.Border.rounded 10
    , Element.padding 20
    , Element.height Element.fill
    , Element.spacing 20
    , Element.width Element.fill
    , Element.centerX
    ]


mapConfigurationFields : List Map.Map -> Maybe String -> (String -> msg) -> Element.Element msg
mapConfigurationFields maps selectedMapId toMessage =
    Element.Lazy.lazy3 mapSelect maps selectedMapId toMessage


mapConfiguration : List Map.Map -> Maybe String -> Element.Element msg
mapConfiguration maps selectedMapId =
    Element.Lazy.lazy2 mapConfigurationInner maps selectedMapId


mapConfigurationInner : List Map.Map -> Maybe String -> Element.Element msg
mapConfigurationInner maps selectedMapId =
    Element.el
        configurationSectionAttributes
        (Element.column
            [ Element.padding 8
            , Element.spacing 20
            , Element.width Element.fill
            ]
            (Element.el [ Element.Font.bold, Element.width Element.fill ] (Element.text "Map")
                :: (maps
                        |> List.map
                            (\map ->
                                let
                                    border =
                                        if Just (map.id |> Map.idToString) == selectedMapId then
                                            [ Element.Border.color (Colors.blue |> Colors.toElementColor)
                                            , Element.Border.solid
                                            , Element.Border.width 2
                                            , Element.Background.color (Colors.lightBlue |> Colors.toElementColor)
                                            , Element.Font.color (Colors.white |> Colors.toElementColor)
                                            ]

                                        else
                                            [ Element.Border.color (Colors.charcoal |> Colors.toElementColor)
                                            , Element.Background.color (Colors.white |> Colors.toElementColor)
                                            , Element.Border.solid
                                            , Element.Border.width 2
                                            ]
                                in
                                Element.row
                                    (Element.width Element.fill :: Element.spacing 10 :: Element.padding 10 :: border)
                                    [ Element.el
                                        [ Element.width (Element.px 50) ]
                                        (Element.Lazy.lazy2 mapView map.countries map.dimensions)
                                    , Element.text map.name
                                    ]
                            )
                   )
            )
        )


mapSelect : List Map.Map -> Maybe String -> (String -> msg) -> Element.Element msg
mapSelect maps selectedMapId toMsg =
    Element.el
        configurationSectionAttributes
        (Element.Input.radio
            [ Element.padding 8
            , Element.spacing 20
            , Element.width Element.fill
            ]
            { onChange = toMsg
            , selected = selectedMapId
            , label = Element.Input.labelAbove [ Element.Font.bold ] (Element.text "Map")
            , options =
                maps
                    |> List.map
                        (\map ->
                            Element.Input.optionWith
                                (map.id |> Map.idToString)
                                (\optionState ->
                                    let
                                        border =
                                            case optionState of
                                                Element.Input.Idle ->
                                                    [ Element.Border.color (Colors.charcoal |> Colors.toElementColor)
                                                    , Element.Background.color (Colors.white |> Colors.toElementColor)
                                                    , Element.Border.solid
                                                    , Element.width Element.fill
                                                    , Element.Border.width 2
                                                    ]

                                                Element.Input.Focused ->
                                                    [ Element.Border.color (Colors.white |> Colors.toElementColor)
                                                    , Element.Border.solid
                                                    , Element.Border.width 2
                                                    , Element.width Element.fill
                                                    ]

                                                Element.Input.Selected ->
                                                    [ Element.Border.color (Colors.blue |> Colors.toElementColor)
                                                    , Element.Border.solid
                                                    , Element.Border.width 2
                                                    , Element.width Element.fill
                                                    , Element.Background.color (Colors.lightBlue |> Colors.toElementColor)
                                                    , Element.Font.color (Colors.white |> Colors.toElementColor)
                                                    ]
                                    in
                                    Element.row
                                        (Element.spacing 10 :: Element.padding 10 :: Element.width Element.fill :: border)
                                        [ Element.el
                                            [ Element.width (Element.px 50) ]
                                            (Element.Lazy.lazy2 mapView map.countries map.dimensions)
                                        , Element.text map.name
                                        ]
                                )
                        )
            }
        )


mapView : Country.Countries -> ( Int, Int ) -> Element.Element msg
mapView countries dimensions =
    Map.view 100 countries dimensions |> Element.html


removePlayerButtonWidth : Element.Attribute msg
removePlayerButtonWidth =
    Element.width (Element.px 60)


removePlayerButton : String -> (String -> msg) -> Element.Element msg
removePlayerButton playerId toMsg =
    Element.el [ removePlayerButtonWidth ]
        (Element.Input.button
            (ViewHelpers.defaultButtonAttributes
                ++ [ Element.Background.color (Colors.red |> Colors.toElementColor)
                   , Element.Font.color (Colors.white |> Colors.toElementColor)
                   , Element.Font.size 10
                   , Element.width Element.shrink
                   ]
            )
            { onPress = Just (toMsg playerId), label = ViewHelpers.centerText "Delete" }
        )


startGameButton : msg -> Element.Element msg
startGameButton message =
    Element.el [ Element.centerX ]
        (Element.Input.button
            (ViewHelpers.defaultButtonAttributes
                ++ [ Element.Background.color (Element.rgb255 0 150 0)
                   , Element.width Element.fill
                   , Element.padding 20
                   , Element.centerX
                   , Element.Font.size 30
                   , Element.Font.color (Colors.white |> ViewHelpers.colorToElementColor)
                   ]
            )
            { onPress = Just message, label = ViewHelpers.centerText "Start Game" }
        )


title : Element.Element msg
title =
    let
        titleMap =
            Map.parse "title" Maps.FracasTitle.map
    in
    Element.el
        [ Element.width (Element.px 400)
        , Element.centerX
        ]
        (Map.view 100 titleMap.countries titleMap.dimensions |> Element.html)
