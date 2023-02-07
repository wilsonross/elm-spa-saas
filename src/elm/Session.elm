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
import Dict exposing (Dict)
import Http
import Port exposing (Cookie)
import Request
import Response exposing (ResponseResult)
import Url



-- MODEL


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
        , cookies = Dict.empty
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


authRefresh : Session -> (ResponseResult -> msg) -> Cmd msg
authRefresh session toMsg =
    let
        bearer =
            Http.header "Authorization:"
                (sessionToCookieToken session)
    in
    Http.request
        { method = "POST"
        , headers = []
        , url = Request.joinUrl (apiUrl session) "/api/collections/users/auth-with-password"
        , body = Http.emptyBody
        , expect = Response.expectStringDetailed toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


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
