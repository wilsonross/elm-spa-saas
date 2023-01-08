module Session exposing (Flags, Session(..), apiUrl, joinUrl, navKey)

import Browser.Navigation as Nav



-- MODEL


type Session
    = Guest { key : Nav.Key, flags : Flags }



-- FLAGS


type alias Flags =
    { apiUrl : String
    }



-- HELPERS


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest { key, flags } ->
            key


apiUrl : Session -> String
apiUrl session =
    case session of
        Guest guest ->
            guest.flags.apiUrl


joinUrl : String -> String -> String
joinUrl url path =
    if String.endsWith "/" url && String.startsWith "/" path then
        String.slice 0 (String.length url - 1) url ++ path

    else if String.endsWith "/" url == False && String.startsWith "/" path == False then
        url ++ "/" ++ path

    else
        url ++ path
