module View exposing (..)

import Html exposing (Attribute, Html, a, button, div, h1, img, input, p, text)
import Html.Attributes exposing (class, href, name, src, type_)
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


viewLogo : List (Attribute msg) -> Html msg
viewLogo attr =
    viewLinkImage
        (class "mr-auto w-[115px] h-[35px]" :: attr)
        "/"
        "/static/img/logo.svg"


viewFooter : Html msg
viewFooter =
    p [] [ text "Footer" ]


viewInput : List (Attribute msg) -> String -> Html msg
viewInput attr placeholder =
    input
        (Html.Attributes.placeholder placeholder
            :: (class <|
                    "focus-visible:outline-none block h-10 w-full rounded"
                        ++ " border border-grey-3 px-[0.93rem]"
                        ++ " placeholder:text-grey-2 text-sm"
               )
            :: attr
        )
        []


viewCheckbox : List (Attribute msg) -> Bool -> String -> Html msg
viewCheckbox attr checked label =
    div (class "flex items-center gap-2" :: attr)
        [ div
            [ class "w-[1.125rem] h-[1.125rem] relative"
            ]
            [ input
                [ class "block w-full h-full opacity-0"
                , type_ "checkbox"
                , Html.Attributes.checked checked
                ]
                []
            , img
                [ src "/static/img/unchecked.svg"
                , class <|
                    "block absolute inset-0 checkbox-img pointer-events-none"
                ]
                []
            , img
                [ src "/static/img/checked.svg"
                , class <|
                    "block absolute inset-0 opacity-0 checkbox-img"
                        ++ " pointer-events-none"
                ]
                []
            ]
        , Html.label
            [ class "text-xs leading-[1.125rem] capitalize"
            ]
            [ text label ]
        ]



-- VIEW AUTH


viewAuthLogo : Html msg
viewAuthLogo =
    viewLogo [ class "mx-auto mb-[1.875rem]" ]


viewTitle : Html msg
viewTitle =
    h1 [ class "text-sm font-bold mb-[0.563rem]" ]
        [ text "Log in"
        ]


viewEmailInput : Html msg
viewEmailInput =
    viewInput [ class "mb-6", type_ "email", name "email" ] "Email"


viewPasswordInput : Html msg
viewPasswordInput =
    viewInput [ class "mb-6", type_ "password", name "password" ] "Password"
