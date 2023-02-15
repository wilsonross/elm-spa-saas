module Api exposing
    ( authRefresh
    , authWithPassword
    , cms
    , cmsSearch
    , create
    , delete
    , linksList
    , requestPasswordReset
    )

import Http exposing (Error)
import Input exposing (Input)
import Json.Decode exposing (Decoder)
import Request
import Response exposing (PaginatedResponse, ResponseResult)
import Session exposing (Session)



-- AUTH


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
            Request.joinUrl (Session.apiUrl session)
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


requestPasswordReset : (Result Error () -> msg) -> Session -> Input -> Cmd msg
requestPasswordReset toMsg session email =
    Http.post
        { url =
            Request.joinUrl
                (Session.apiUrl session)
                "/api/collections/users/request-password-reset"
        , body =
            Http.jsonBody
                (Input.encodeInput
                    [ ( "email", email )
                    ]
                )
        , expect = Http.expectWhatever toMsg
        }


delete : (Result Error () -> msg) -> Session -> Cmd msg
delete toMsg session =
    Http.request
        { method = "DELETE"
        , headers =
            [ Http.header
                "Authorization"
                (Session.sessionToCookieToken session)
            ]
        , url =
            Request.joinUrl
                (Session.apiUrl session)
                "/api/collections/users/records/"
                ++ Session.userId session
        , body = Http.emptyBody
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }



-- CMS


cms : Decoder response -> (Result Http.Error response -> msg) -> Session -> String -> Cmd msg
cms decoder toMsg session id =
    Http.get
        { url =
            Request.joinUrl
                (Session.apiUrl session)
                ("/api/collections/cms/records/" ++ id)
        , expect = Http.expectJson toMsg decoder
        }


cmsSearch : Decoder response -> (Result Http.Error (PaginatedResponse (List response)) -> msg) -> Session -> String -> Cmd msg
cmsSearch decoder toMsg session query =
    Http.get
        { url =
            Request.joinUrl
                (Session.apiUrl session)
                (searchBuilder query)
        , expect =
            Http.expectJson
                toMsg
                (Response.decodePaginatedResponse decoder)
        }



-- LINKS


linksList : Decoder response -> (Result Http.Error (PaginatedResponse (List response)) -> msg) -> Session -> Cmd msg
linksList decoder toMsg session =
    Http.get
        { url =
            Request.joinUrl
                (Session.apiUrl session)
                "/api/collections/links/records"
        , expect =
            Http.expectJson
                toMsg
                (Response.decodePaginatedResponse decoder)
        }



-- HELPERS


searchBuilder : String -> String
searchBuilder query =
    "/api/collections/cms/records?filter=(title~'"
        ++ query
        ++ "'||tagline~'"
        ++ query
        ++ "')"
