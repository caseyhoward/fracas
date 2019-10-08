module ViewHelpers exposing
    ( centerText
    , colorToElementColor
    , defaultButtonAttributes
    , defaultLabelAttributes
    , defaultTextInputAttributes
    , errorToString
    ,  pixelsPerMapSquare
       -- , selectButton

    )

import Colors
import Color
import Element
import Element.Background
import Element.Border
import Element.Font
import Graphql.Http
import Graphql.Http.GraphqlError


centerText : String -> Element.Element msg
centerText text =
    Element.el [ Element.centerX ] (Element.text text)


colorToElementColor : Colors.Color -> Element.Color
colorToElementColor color =
    color |> Colors.toColor |> Color.toRgba |> Element.fromRgb


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


defaultLabelAttributes : List (Element.Attribute msg)
defaultLabelAttributes =
    [ Element.Font.size 12
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


pixelsPerMapSquare : Int
pixelsPerMapSquare =
    100



-- selectButton : String -> Element.Element msg -> msg -> Element.Element msg
-- selectButton labelString selectedItem message =
--     Element.column
--         [ Element.width Element.fill, Element.height Element.fill ]
--         [ Element.Input.button
--             ([ Element.Border.width 1
--              , Element.inFront
--                 (Element.el
--                     labelAttributes
--                     (Element.text labelString)
--                 )
--              ]
--                 ++ defaultTextInputAttributes
--             )
--             { label =
--                 Element.row
--                     [ Element.width Element.fill ]
--                     [ Element.el
--                         [ Element.width (Element.px 200), Element.clipX ]
--                         selectedItem
--                     , Element.el
--                         [ Element.alignRight ]
--                         -- (fontAwesomeIconSolid "angle-down")
--                         (Element.text ".")
--                     ]
--             , onPress = Just message
--             }
--         , errorMessage ""
--         ]
-- labelAttributes =
--     [ Element.Font.size 12
--     , Element.moveUp 7
--     , Element.moveRight 5
--     -- , Element.Background.color white
--     , Element.paddingEach { left = 5, top = 0, right = 5, bottom = 0 }
--     ]
-- errorMessage : String -> Element.Element msg
-- errorMessage error =
--     Element.el
--         [ Element.height (Element.px 20)
--         , Element.Font.color (Element.rgb255 255 0 0)
--         , Element.Font.size 12
--         ]
--         (Element.text error)


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError httpError ->
            case httpError of
                Graphql.Http.BadUrl url ->
                    "Http error: Bad url - " ++ url

                Graphql.Http.Timeout ->
                    "Http error: timeout"

                Graphql.Http.NetworkError ->
                    "Http error: network error"

                Graphql.Http.BadStatus _ string ->
                    "Http error: bad status - " ++ string

                Graphql.Http.BadPayload _ ->
                    "Http error: bad payload"


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message
