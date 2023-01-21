module Session exposing
    ( Flags
    , Session(..)
    , apiUrl
    , init
    , joinUrl
    , navKey
    , pathFromSession
    , updateSessionPath
    )

import Browser.Navigation as Nav
import Url



-- MODEL


type Session
    = Guest { key : Nav.Key, flags : Flags, path : String }


init : Flags -> Nav.Key -> String -> Session
init flags key path =
    Guest { key = key, flags = flags, path = path }



-- FLAGS


type alias Flags =
    { apiUrl : String
    }



-- HELPERS


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest { key } ->
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


updateSessionPath : Url.Url -> Session -> Session
updateSessionPath url session =
    case session of
        Guest guest ->
            Guest
                { guest | path = url.path }


pathFromSession : Session -> String
pathFromSession session =
    case session of
        Guest guest ->
            guest.path
