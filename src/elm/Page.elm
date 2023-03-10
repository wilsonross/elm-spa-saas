module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html)



-- VIEW


view : { title : String, content : Html msg } -> Document msg
view { title, content } =
    { title = title
    , body = [ content ]
    }


viewPage : (a -> msg) -> { title : String, content : Html a } -> Document msg
viewPage toMsg { title, content } =
    { title = title
    , body = [ Html.map toMsg content ]
    }


viewComponent : (a -> msg) -> Html a -> Html msg
viewComponent toMsg content =
    Html.map toMsg content
