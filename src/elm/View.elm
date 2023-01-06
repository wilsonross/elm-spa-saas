module View exposing (..)

import Html exposing (Html, a, button, div, form, img, input, li, p, text, ul)
import Html.Attributes exposing (href, placeholder, src)



-- TYPES


type alias Link =
    { url : String
    , title : String
    }



-- VIEW FUNCTIONS


viewLink : String -> String -> Html msg
viewLink url content =
    a [ href url ] [ text content ]


viewLinkImage : String -> String -> Html msg
viewLinkImage url image =
    a [ href url ] [ img [ src image ] [] ]


viewHeader : List Link -> Html msg
viewHeader navLinks =
    div []
        [ viewLinkImage "/" "/static/img/logo.svg"
        , form
            []
            [ input [ placeholder "Find design" ] []
            , button []
                [ img [ src "/static/img/search.svg" ] []
                ]
            ]
        , div []
            [ viewLinkImage "/login" "/static/img/login.svg"
            , viewLinkImage "/register" "/static/img/register.svg"
            ]
        , button []
            [ img [ src "/static/img/menu.svg" ] []
            ]
        , ul []
            (List.map (\link -> li [] [ viewLink link.url link.title ]) navLinks)
        ]


viewFooter : Html msg
viewFooter =
    p [] [ text "Footer" ]
