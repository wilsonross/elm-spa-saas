module Session exposing
    ( Flags
    , Session(..)
    , apiUrl
    , init
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
