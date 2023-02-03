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
import Cookie exposing (Cookie)
import Url



-- MODEL


type alias Flags =
    { apiUrl : String
    }


type alias GuestSession =
    { key : Nav.Key
    , flags : Flags
    , path : String
    , cookies : List Cookie
    }


type Session
    = Guest GuestSession


init : Flags -> Nav.Key -> String -> Session
init flags key path =
    Guest
        { key = key
        , flags = flags
        , path = path
        , cookies = []
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
