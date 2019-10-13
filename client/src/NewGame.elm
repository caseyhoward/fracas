module NewGame exposing
    ( addPlayerButton
    , colorButton
    , mapConfiguration
    , mapView
    , removePlayerButton
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


mapConfiguration : List Map.Map -> Maybe String -> (String -> msg) -> Element.Element msg
mapConfiguration maps selectedMapId toMessage =
    Element.Lazy.lazy3 mapSelect maps selectedMapId toMessage


mapSelect : List Map.Map -> Maybe String -> (String -> msg) -> Element.Element msg
mapSelect maps selectedMapId toMsg =
    Element.el
        [ Element.centerX
        , Element.Background.color (Colors.gray |> Colors.toElementColor)
        , Element.Border.rounded 10
        , Element.padding 20
        ]
        (Element.Input.radio
            [ Element.padding 8
            , Element.spacing 20
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
                                                    , Element.Border.width 2
                                                    ]

                                                Element.Input.Focused ->
                                                    [ Element.Border.color (Colors.white |> Colors.toElementColor)
                                                    , Element.Border.solid
                                                    , Element.Border.width 2
                                                    ]

                                                Element.Input.Selected ->
                                                    [ Element.Border.color (Colors.blue |> Colors.toElementColor)
                                                    , Element.Border.solid
                                                    , Element.Border.width 2
                                                    , Element.Background.color (Colors.lightBlue |> Colors.toElementColor)
                                                    , Element.Font.color (Colors.white |> Colors.toElementColor)
                                                    ]
                                    in
                                    Element.row
                                        (Element.spacing 10 :: Element.padding 10 :: Element.width (Element.px 300) :: border)
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


removePlayerButton : Int -> (Int -> msg) -> Element.Element msg
removePlayerButton playerId toMsg =
    Element.Input.button
        (ViewHelpers.defaultButtonAttributes
            ++ [ Element.Background.color (Colors.red |> Colors.toElementColor)
               , Element.Font.color (Colors.white |> Colors.toElementColor)
               , Element.Font.size 10
               ]
        )
        { onPress = Just (toMsg playerId), label = Element.text "Delete" }


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
