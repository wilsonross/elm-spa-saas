module View exposing
    ( Link
    , delay
    , viewAlternative
    , viewAuthLogo
    , viewButtonImage
    , viewDescription
    , viewErrors
    , viewFooter
    , viewLink
    , viewLinkImage
    , viewLogo
    , viewRule
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
        , h3
        , hr
        , img
        , p
        , span
        , text
        )
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (custom)
import Json.Decode as Decode
import List exposing (length)
import Process
import Request exposing (Status(..))
import Response exposing (ErrorMessage)
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


viewRule : Html msg
viewRule =
    hr
        [ class "max-w-[3rem] w-full mx-auto border-grey-3 my-11" ]
        []


viewDescription : String -> Html msg
viewDescription description =
    h3
        [ class "text-center text-xl font-medium mb-[0.625rem]" ]
        [ text description ]



-- VIEW AUTH


viewAuthLogo : Html msg
viewAuthLogo =
    viewLogo [ class "mx-auto mb-[1.875rem]" ]


viewTitle : String -> Html msg
viewTitle title =
    h1 [ class "text-sm font-bold mb-[0.563rem]" ]
        [ text title
        ]


viewAlternative : String -> String -> String -> String -> Html msg
viewAlternative prefix focus suffix href =
    div [ class "text-xs leading-[1.125rem] text-center" ]
        [ span [] [ text (prefix ++ " ") ]
        , viewLink [ class "text-turq" ] href focus
        , span [] [ text (" " ++ suffix) ]
        ]



-- HELPERS


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity
