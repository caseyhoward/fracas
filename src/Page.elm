module Page exposing (Page(..), view)

import Browser
import Html


type Page
    = ActiveGame


view : Page -> { title : String, content : Html.Html msg } -> Browser.Document msg
view page { title, content } =
    { title = title ++ " - Fracas"
    , body = [ content ]
    }
