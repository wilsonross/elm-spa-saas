module Page exposing (view)

import Browser exposing (Document)
import Html exposing (..)



-- VIEW


view : { title : String, content : Html msg } -> Document msg
view { title, content } =
    { title = title
    , body = [ content ]
    }
