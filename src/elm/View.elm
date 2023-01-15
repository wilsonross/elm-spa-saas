module View exposing (..)

import Html exposing (Attribute, Html, a, button, img, p, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (custom)
import Json.Decode as Decode



-- TYPES


type alias Link =
    { url : String
    , title : String
    , sortOrder : Int
    }



-- VIEW FUNCTIONS


viewLink : List (Attribute msg) -> String -> String -> Html msg
viewLink attr url content =
    a (href url :: attr) [ text content ]


viewLinkImage : List (Attribute msg) -> String -> String -> Html msg
viewLinkImage attr url image =
    a (href url :: (class "block" :: attr))
        [ img
            [ src image
            , class "block h-full w-full"
            ]
            []
        ]


viewButtonImage : List (Attribute msg) -> msg -> String -> Html msg
viewButtonImage attr click image =
    let
        options =
            Decode.succeed
                { message = click
                , stopPropagation = True
                , preventDefault = True
                }
    in
    button (custom "click" options :: (class "block shrink-0" :: attr))
        [ img
            [ src image
            , class "block h-full w-full"
            ]
            []
        ]


viewLogo : Html msg
viewLogo =
    viewLinkImage
        [ class "mr-auto w-[115px] h-[35px]"
        ]
        "/"
        "/static/img/logo.svg"


viewFooter : Html msg
viewFooter =
    p [] [ text "Footer" ]
