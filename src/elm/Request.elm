module Request exposing (Status(..), joinUrl)

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

    else if String.endsWith "/" url == False && String.startsWith "/" path == False then
        url ++ "/" ++ path

    else
        url ++ path
