module Auth exposing
    ( authRefresh
    , authWithPassword
    , create
    )

import Http
import Input exposing (Input)
import Request
import Response exposing (ResponseResult)
import Session exposing (Session)



-- HELPERS


authRefresh : (ResponseResult -> msg) -> Session -> Cmd msg
authRefresh toMsg session =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header
                "Authorization"
                (Session.sessionToCookieToken session)
            ]
        , url =
            Request.joinUrl
                (Session.apiUrl session)
                "/api/collections/users/auth-refresh"
        , body = Http.emptyBody
        , expect = Response.expectStringDetailed toMsg
        , timeout = Nothing
        , tracker = Nothing
        }


authWithPassword : (ResponseResult -> msg) -> Session -> Input -> Input -> Cmd msg
authWithPassword toMsg session identity password =
    Http.post
        { url =
            Request.joinUrl
                (Session.apiUrl session)
                "/api/collections/users/auth-with-password"
        , body =
            Http.jsonBody
                (Input.encodeInput
                    [ ( "identity", identity )
                    , ( "password", password )
                    ]
                )
        , expect = Response.expectStringDetailed toMsg
        }


create : (ResponseResult -> msg) -> Session -> Input -> Input -> Input -> Input -> Input -> Cmd msg
create toMsg session email password passwordConfirm firstName lastName =
    Http.post
        { url =
            Request.joinUrl
                (Session.apiUrl session)
                "/api/collections/users/records"
        , body =
            Http.jsonBody
                (Input.encodeInput
                    [ ( "email", email )
                    , ( "password", password )
                    , ( "passwordConfirm", passwordConfirm )
                    , ( "firstName", firstName )
                    , ( "lastName", lastName )
                    ]
                )
        , expect = Response.expectStringDetailed toMsg
        }
