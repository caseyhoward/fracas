module Page exposing
    ( Page(..)
    , view
    )

import Browser
import Html


type Page
    = Game
    | NewGame
    | InternetGame
    | NewMap


view : Page -> { title : String, content : Html.Html msg } -> Browser.Document msg
view _ { title, content } =
    { title = "Fracas - " ++ title
    , body = [ content ]
    }
