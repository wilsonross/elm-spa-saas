module Auth exposing (authRefresh)

import Http
import Request
import Response exposing (ResponseResult)
import Session exposing (Session)



-- HELPERS


authRefresh : Session -> (ResponseResult -> msg) -> Cmd msg
authRefresh session toMsg =
    let
        bearer =
            Http.header "Authorization:"
                (Session.sessionToCookieToken session)
    in
    Http.request
        { method = "POST"
        , headers = [ bearer ]
        , url = Request.joinUrl (Session.apiUrl session) "/api/collections/users/auth-with-password"
        , body = Http.emptyBody
        , expect = Response.expectStringDetailed toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
