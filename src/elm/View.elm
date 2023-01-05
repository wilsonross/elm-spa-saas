module View exposing (..)

import Html exposing (Html, a, p, text)
import Html.Attributes exposing (..)



-- VIEW FUNCTIONS


viewLink : String -> String -> Html msg
viewLink url content =
    a [ href url ] [ text content ]


viewHeader : Html msg
viewHeader =
    p [] [ text "Header" ]


viewFooter : Html msg
viewFooter =
    p [] [ text "Footer" ]
