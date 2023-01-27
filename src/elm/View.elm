module View exposing
    ( Link
    , delay
    , viewAuthLogo
    , viewButtonImage
    , viewCheckbox
    , viewErrors
    , viewFooter
    , viewInput
    , viewLink
    , viewLinkImage
    , viewLogo
    , viewTitle
    )

import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , h1
        , img
        , input
        , p
        , span
        , text
        )
import Html.Attributes exposing (class, href, name, src, type_)
import Html.Events exposing (custom, onInput)
import Json.Decode as Decode
import List exposing (length)
import Process
import Request exposing (ErrorMessage)
import Task



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


viewInput : List (Attribute msg) -> String -> (String -> msg) -> Html msg
viewInput attr placeholder msg =
    input
        (onInput msg
            :: (Html.Attributes.placeholder placeholder
                    :: (class <|
                            "focus-visible:outline-none block h-10 w-full"
                                ++ " border border-grey-3 px-[0.93rem] rounded"
                                ++ " placeholder:text-grey-2 text-sm"
                                ++ " transition-colors duration-700 ease-out"
                       )
                    :: attr
               )
        )
        []


viewCheckbox : List (Attribute msg) -> Bool -> String -> String -> Html msg
viewCheckbox attr checked name label =
    div (class "flex items-center gap-2" :: attr)
        [ div
            [ class "w-[1.125rem] h-[1.125rem] relative"
            ]
            [ viewCheckboxHidden checked
            , viewCheckboxImageUnchecked
            , viewCheckboxImageChecked
            ]
        , viewLabel name label
        ]


viewCheckboxHidden : Bool -> Html msg
viewCheckboxHidden checked =
    input
        [ class "block w-full h-full opacity-0"
        , type_ "checkbox"
        , Html.Attributes.checked checked
        ]
        []


viewCheckboxImageUnchecked : Html msg
viewCheckboxImageUnchecked =
    img
        [ src "/static/img/unchecked.svg"
        , class <|
            "block absolute inset-0 checkbox-img pointer-events-none"
        ]
        []


viewCheckboxImageChecked : Html msg
viewCheckboxImageChecked =
    img
        [ src "/static/img/checked.svg"
        , class <|
            "block absolute inset-0 opacity-0 checkbox-img pointer-events-none"
        ]
        []


viewLabel : String -> String -> Html msg
viewLabel name label =
    Html.label
        [ class "text-xs leading-[1.125rem] capitalize"
        , Html.Attributes.name name
        ]
        [ text label ]


viewErrors : List ErrorMessage -> Html msg
viewErrors errors =
    if length errors > 0 then
        div
            [ class <|
                "fixed left-1/2 bottom-6 -translate-x-1/2 animate-errors"
            ]
            (List.map viewError errors)

    else
        text ""


viewError : ErrorMessage -> Html msg
viewError error =
    div
        [ class <|
            "bg-red-500 flex shadow-modal rounded gap-2 py-[0.72rem] mb-2 pr-4"
                ++ " last:mb-0 pl-3"
        ]
        [ img [ src "/static/img/info.svg" ] []
        , span [ class "text-white font-medium" ] [ text error.message ]
        ]



-- VIEW AUTH


viewAuthLogo : Html msg
viewAuthLogo =
    viewLogo [ class "mx-auto mb-[1.875rem]" ]


viewTitle : String -> Html msg
viewTitle title =
    h1 [ class "text-sm font-bold mb-[0.563rem]" ]
        [ text title
        ]



-- HELPERS


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
