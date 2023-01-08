module Page exposing (view)

import Browser exposing (Document)
import Html exposing (Html, div)
import View exposing (viewFooter, viewHeader)



-- VIEW


view : { title : String, content : Html msg } -> Document msg
view { title, content } =
    { title = title
    , body = [ content ]
    }
