module Session exposing
    ( Flags
    , Session(..)
    , apiUrl
    , initGuest
    , navKey
    , pathFromSession
    , updateSessionCookies
    , updateSessionPath
    )

import Browser.Navigation as Nav
import Port exposing (Cookie)
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


type alias UserSession =
    { key : Nav.Key
    , flags : Flags
    , path : String
    , cookies : List Cookie
    , jwt : String
    , id : String
    , email : String
    , firstName : String
    , lastName : String
    }


type Session
    = Guest GuestSession
    | User UserSession


initGuest : Flags -> Nav.Key -> String -> Session
initGuest flags key path =
    Guest
        { key = key
        , flags = flags
        , path = path
        , cookies = []
        }


initUser : GuestSession -> String -> String -> String -> String -> String -> Session
initUser guest jwt id email firstName lastName =
    User
        { key = guest.key
        , flags = guest.flags
        , path = guest.path
        , cookies = guest.cookies
        , jwt = jwt
        , id = id
        , email = email
        , firstName = firstName
        , lastName = lastName
        }



-- HELPERS


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest { key } ->
            key

        User { key } ->
            key


apiUrl : Session -> String
apiUrl session =
    case session of
        Guest guest ->
            guest.flags.apiUrl

        User user ->
            user.flags.apiUrl


updateSessionCookies : Cookie -> Session -> Session
updateSessionCookies cookie session =
    case session of
        Guest guest ->
            Guest
                { guest | cookies = cookie :: guest.cookies }

        User user ->
            User
                { user | cookies = cookie :: user.cookies }


updateSessionPath : Url.Url -> Session -> Session
updateSessionPath url session =
    case session of
        Guest guest ->
            Guest
                { guest | path = url.path }

        User user ->
            User
                { user | path = url.path }


pathFromSession : Session -> String
pathFromSession session =
    case session of
        Guest guest ->
            guest.path

        User user ->
            user.path
