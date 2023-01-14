module View exposing (..)

import Html exposing (Attribute, Html, a, button, div, form, img, input, p, text)
import Html.Attributes exposing (class, href, placeholder, src)
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


viewHeader : Html msg -> Html msg -> Html msg -> Html msg -> Html msg
viewHeader nav overlay navButton mobileSearch =
    div
        [ class <|
            "flex h-20 mx-auto px-9 lg:px-5 items-center max-w-[76.5rem]"
                ++ " w-full gap-9"
        ]
        [ viewLogo
        , viewDesktopSearch
        , mobileSearch
        , viewAuthLinks
        , navButton
        , overlay
        , nav
        ]


viewLogo : Html msg
viewLogo =
    viewLinkImage
        [ class "mr-auto w-[115px] h-[35px]"
        ]
        "/"
        "/static/img/logo.svg"


viewDesktopSearch : Html msg
viewDesktopSearch =
    form
        [ class <|
            "bg-grey-0 rounded-md px-[11px] w-[16.625rem] h-10 hidden md:flex"
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


viewAuthLinks : Html msg
viewAuthLinks =
    div [ class "flex gap-9" ]
        [ viewLinkImage [ class "w-[41px] h-[18px]" ]
            "/login"
            "/static/img/login.svg"
        , viewLinkImage [ class "w-[61px] h-[18px]" ]
            "/register"
            "/static/img/register.svg"
        ]


viewFooter : Html msg
viewFooter =
    p [] [ text "Footer" ]
