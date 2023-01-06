module Session exposing (Flags, Session(..), navKey)

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
