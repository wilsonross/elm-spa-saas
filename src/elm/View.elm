module View exposing (..)

import Html exposing (Attribute, Html, a, button, div, form, img, input, li, p, text, ul)
import Html.Attributes exposing (class, href, placeholder, src)



-- TYPES


type alias Link =
    { url : String
    , title : String
    }



-- VIEW FUNCTIONS


viewLink : String -> String -> Html msg
viewLink url content =
    a [ href url ] [ text content ]


viewLinkImage : List (Attribute msg) -> String -> String -> Html msg
viewLinkImage attr url image =
    a (href url :: (class "block" :: attr))
        [ img
            [ src image
            , class "block h-full w-full"
            ]
            []
        ]


viewHeader : Html msg -> Html msg
viewHeader navLinks =
    div
        [ class <|
            "flex h-20 mx-auto px-5 items-center max-w-[76.5rem] w-full gap-9"
        ]
        [ viewLinkImage
            [ class "mr-auto w-[115px] h-[35px] block"
            ]
            "/"
            "/static/img/logo.svg"
        , form
            [ class <|
                "bg-grey-0 rounded-md flex px-[11px] w-[16.625rem] h-10"
                    ++ " items-center"
            ]
            [ input
                [ placeholder "Find design"
                , class <|
                    "bg-grey-0 focus-visible:outline-none w-full pt-[2px]"
                        ++ " placeholder:grey-2"
                ]
                []
            , button [ class "h-5 w-5 shrink-0" ]
                [ img [ src "/static/img/search.svg" ] []
                ]
            ]
        , div [ class "flex gap-9" ]
            [ viewLinkImage [ class "w-[41px] h-[18px]" ]
                "/login"
                "/static/img/login.svg"
            , viewLinkImage [ class "w-[61px] h-[18px]" ]
                "/register"
                "/static/img/register.svg"
            ]
        , button [ class "block w-6 h-6" ]
            [ img
                [ src "/static/img/menu.svg"
                , class "block w-full h-full"
                ]
                []
            ]
        , div
            [ class <|
                "fixed top-0 right-0 bottom-0 w-[21.25rem] translate-x-full"
            ]
            [ navLinks ]
        ]


viewFooter : Html msg
viewFooter =
    p [] [ text "Footer" ]
