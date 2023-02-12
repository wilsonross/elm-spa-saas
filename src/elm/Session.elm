module Session exposing
    ( Cookie
    , Flags
    , Session(..)
    , apiUrl
    , initGuest
    , isUserAuth
    , navKey
    , pathFromSession
    , rememberMe
    , sessionToCookieToken
    , updateSessionPath
    , updateSessionVariant
    , updateSessionWithCookie
    , updateSessionWithJson
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Response exposing (AuthResponse, JsonResponse(..))
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


updateSessionWithCookie : Cookie -> Session -> Session
updateSessionWithCookie ( key, value ) session =
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


updateSessionWithJson : Session -> JsonResponse badResponse AuthResponse -> Session
updateSessionWithJson session response =
    case response of
        JsonSuccess res ->
            updateSessionVariant res session

        _ ->
            session


updateSessionVariant : AuthResponse -> Session -> Session
updateSessionVariant authResponse session =
    case session of
        Guest guest ->
            initUser
                guest
                authResponse.token
                authResponse.record.id
                authResponse.record.firstName
                authResponse.record.lastName

        User user ->
            User user


isUserAuth : Session -> Bool
isUserAuth session =
    case session of
        Guest _ ->
            False

        User _ ->
            True


rememberMe : Bool -> Int
rememberMe remember =
    if remember then
        30

    else
        0
