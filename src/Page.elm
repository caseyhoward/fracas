module Page exposing
    ( Page(..)
    , view
    )

import Browser
import Html


type Page
    = ActiveGame
    | NewGame
    | EditMap
    | NewMap
    | Game


view : Page -> { title : String, content : Html.Html msg } -> Browser.Document msg
view _ { title, content } =
    { title = title ++ " - Fracas"
    , body = [ content ]
    }
