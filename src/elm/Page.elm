module Page exposing (view, viewHeaderFooter)

import Browser exposing (Document)
import Html exposing (Html, div)
import View exposing (viewFooter, viewHeader)



-- VIEW


view : { title : String, content : Html msg } -> Document msg
view { title, content } =
    { title = title
    , body = [ content ]
    }


viewHeaderFooter : { title : String, content : Html msg } -> { title : String, content : Html msg }
viewHeaderFooter { title, content } =
    { title = title
    , content =
        div []
            [ viewHeader [ { url = "/", title = "Home" } ]
            , content
            , viewFooter
            ]
    }
