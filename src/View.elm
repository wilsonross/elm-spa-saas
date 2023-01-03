module View exposing (..)

import Html exposing (Html, a, text)
import Html.Attributes exposing (..)



-- VIEW FUNCTIONS


viewLink : String -> String -> Html msg
viewLink url content =
    a [ href url ] [ text content ]
