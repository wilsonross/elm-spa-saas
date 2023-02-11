port module Port exposing
    ( Msg
    , addGetSession
    , getSession
    , recieveSession
    , setSession
    , subscriptions
    , update
    )

import Auth
import Json.Decode as Decode
import Response
    exposing
        ( AuthResponse
        , JsonResponse(..)
        , ResponseResult
        )
import Session exposing (Cookie, Session(..))



-- PORTS


port setSession : ( String, Int ) -> Cmd msg


port getSession : () -> Cmd msg


port recieveSession : (( String, String ) -> msg) -> Sub msg



-- UPDATE


type Msg
    = RecieveSession Cookie
    | GotAuthRefreshResponse ResponseResult


update : Session -> Msg -> ( Session, Cmd Msg )
update session msg =
    case msg of
        RecieveSession cookie ->
            let
                updatedSession =
                    Session.updateSessionCookies cookie session
            in
            ( updatedSession
            , updatedSession |> Auth.authRefresh GotAuthRefreshResponse
            )

        GotAuthRefreshResponse response ->
            ( updateWithResponse response session, Cmd.none )


updateWithResponse : ResponseResult -> Session -> Session
updateWithResponse result session =
    case result of
        Ok ( _, res ) ->
            stringToJson res
                |> updateSession session

        Err _ ->
            session



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    recieveSession RecieveSession



-- HELPERS


addGetSession : ( model, Cmd msg ) -> ( model, Cmd msg )
addGetSession ( model, cmd ) =
    ( model, Cmd.batch [ cmd, getSession () ] )



-- JSON


type alias AuthRefreshJsonResponse =
    JsonResponse {} AuthResponse


stringToJson : String -> AuthRefreshJsonResponse
stringToJson str =
    Response.stringToJson
        (Decode.succeed {})
        Response.decodeAuthResponse
        str


updateSession : Session -> AuthRefreshJsonResponse -> Session
updateSession session response =
    case response of
        JsonSuccess res ->
            changeSessionVariant res session

        _ ->
            session


changeSessionVariant : AuthResponse -> Session -> Session
changeSessionVariant authResponse session =
    let
        record =
            authResponse.record
    in
    case session of
        Guest guest ->
            Session.initUser guest authResponse.token record.id record.firstName record.lastName

        User user ->
            User user
