module Session exposing (Session(..), navKey)

import Browser.Navigation as Nav



-- MODEL


type Session
    = Guest Nav.Key



-- HELPERS


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key
