module Session exposing
    ( Cookie
    , Flags
    , Session(..)
    , apiUrl
    , initGuest
    , initUser
    , navKey
    , pathFromSession
    , sessionToCookieToken
    , updateSessionCookies
    , updateSessionPath
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Url



-- MODEL


type alias Cookie =
    ( String, String )


type alias Flags =
    { apiUrl : String
    }


type alias GuestSession =
    { key : Nav.Key
    , flags : Flags
    , path : String
    , cookies : Dict String String
    }


type alias UserSession =
    { key : Nav.Key
    , flags : Flags
    , path : String
    , cookies : Dict String String
    , jwt : String
    , id : String
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
        , cookies = Dict.empty
        }


initUser : GuestSession -> String -> String -> String -> String -> Session
initUser guest jwt id firstName lastName =
    User
        { key = guest.key
        , flags = guest.flags
        , path = guest.path
        , cookies = guest.cookies
        , jwt = jwt
        , id = id
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
updateSessionCookies ( key, value ) session =
    case session of
        Guest guest ->
            Guest
                { guest | cookies = Dict.insert key value guest.cookies }

        User user ->
            User
                { user | cookies = Dict.insert key value user.cookies }


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


sessionToCookieToken : Session -> String
sessionToCookieToken session =
    case session of
        Guest guest ->
            guest.cookies
                |> Dict.get "session"
                |> Maybe.withDefault ""

        User user ->
            user.cookies
                |> Dict.get "session"
                |> Maybe.withDefault ""
