module Session exposing
    ( Cookie
    , Flags
    , Session(..)
    , forceGuestSession
    , initGuest
    , initLoading
    , navKey
    , pathFromSession
    , rememberMe
    , sessionToCookieToken
    , toFlag
    , updateSessionPath
    , updateSessionWithCookie
    , updateSessionWithJson
    , userId
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Response exposing (AuthResponse, JsonResponse(..))
import Url



-- MODEL


type alias Cookie =
    ( String, String, Int )


type alias Flags =
    { apiUrl : String
    , cmsAccountId : String
    , cmsCtaId : String
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
    = Loading GuestSession
    | Guest GuestSession
    | User UserSession


initLoading : Flags -> Nav.Key -> String -> Session
initLoading flags key path =
    Loading
        { key = key
        , flags = flags
        , path = path
        , cookies = Dict.empty
        }


initGuest : GuestSession -> Session
initGuest guestSession =
    Guest guestSession


initUser : GuestSession -> String -> String -> String -> String -> String -> Session
initUser guest jwt id email firstName lastName =
    User
        { key = guest.key
        , flags = guest.flags
        , path = guest.path
        , cookies = guest.cookies
        , jwt = jwt
        , email = email
        , id = id
        , firstName = firstName
        , lastName = lastName
        }



-- HELPERS


navKey : Session -> Nav.Key
navKey session =
    case session of
        Loading { key } ->
            key

        Guest { key } ->
            key

        User { key } ->
            key


toFlag : Session -> (Flags -> a) -> a
toFlag session flag =
    case session of
        Loading guest ->
            flag guest.flags

        Guest guest ->
            flag guest.flags

        User user ->
            flag user.flags


updateSessionWithCookie : Cookie -> Session -> Session
updateSessionWithCookie ( key, value, expiry ) session =
    case session of
        Loading guest ->
            Loading
                { guest | cookies = Dict.insert key value guest.cookies }

        Guest guest ->
            Guest
                { guest | cookies = Dict.insert key value guest.cookies }

        User user ->
            User
                { user | cookies = Dict.insert key value user.cookies }


updateSessionPath : Url.Url -> Session -> Session
updateSessionPath url session =
    case session of
        Loading guest ->
            Loading
                { guest | path = url.path }

        Guest guest ->
            Guest
                { guest | path = url.path }

        User user ->
            User
                { user | path = url.path }


pathFromSession : Session -> String
pathFromSession session =
    case session of
        Loading guest ->
            guest.path

        Guest guest ->
            guest.path

        User user ->
            user.path


sessionToCookieToken : Session -> String
sessionToCookieToken session =
    case session of
        Loading guest ->
            guest.cookies
                |> Dict.get "session"
                |> Maybe.withDefault ""

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
    case session of
        Loading guest ->
            case response of
                JsonSuccess res ->
                    initUser
                        guest
                        res.token
                        res.record.id
                        res.record.email
                        res.record.firstName
                        res.record.lastName

                _ ->
                    initGuest guest

        Guest guest ->
            case response of
                JsonSuccess res ->
                    initUser
                        guest
                        res.token
                        res.record.id
                        res.record.email
                        res.record.firstName
                        res.record.lastName

                _ ->
                    session

        User _ ->
            session


forceGuestSession : Session -> Session
forceGuestSession session =
    case session of
        Loading guest ->
            initGuest guest

        User user ->
            initGuest
                { key = user.key
                , flags = user.flags
                , path = user.path
                , cookies = user.cookies
                }

        _ ->
            session


daysToMs : Int -> Int
daysToMs days =
    days * 24 * 60 * 60 * 1000


rememberMe : Bool -> Int
rememberMe remember =
    if remember then
        daysToMs 30

    else
        0


userId : Session -> String
userId session =
    case session of
        User user ->
            user.id

        _ ->
            ""
