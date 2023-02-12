module Request exposing (Status(..), joinUrl, viewPreloader)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)



-- MODEL


type Status res
    = None
    | Failure
    | Loading
    | Response res



-- HELPERS


joinUrl : String -> String -> String
joinUrl url path =
    if String.endsWith "/" url && String.startsWith "/" path then
        String.slice 0 (String.length url - 1) url ++ path

    else if
        String.endsWith "/" url
            == False
            && String.startsWith "/" path
            == False
    then
        url ++ "/" ++ path

    else
        url ++ path



-- VIEW


viewPreloader : Status res -> Html msg
viewPreloader status =
    case status of
        Loading ->
            div
                [ class <|
                    "w-full h-full bg-white absolute left-1/2 top-1/2 flex"
                        ++ " -translate-x-1/2 -translate-y-1/2 z-10"
                        ++ " items-center justify-center"
                ]
                [ div
                    [ class <|
                        "w-20 h-20 border-grey-1 border-[5px] border-t-turq"
                            ++ " animate-spin rounded-full"
                    ]
                    []
                ]

        _ ->
            text ""
