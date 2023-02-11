module Main exposing (main)

import Auth
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Page
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Port
import Response
    exposing
        ( AuthResponse
        , JsonResponse(..)
        , ResponseResult
        )
import Route
import Session exposing (Cookie, Flags, Session(..))
import Url



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Model
    = Home Home.Model
    | Login Login.Model
    | Register Register.Model


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( home, _ ) =
            Home.init (Session.initGuest flags key url.path)
    in
    changeRouteTo url (Home home) |> Port.addGetSession



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RecieveSession Cookie
    | GotAuthRefreshResponse ResponseResult
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotRegisterMsg Register.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl
                        (Session.navKey (toSession model))
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo url model

        ( RecieveSession cookie, _ ) ->
            let
                updatedSession =
                    Session.updateSessionWithCookie
                        cookie
                        (toSession model)
            in
            ( updatedSession |> updateModelWithSession model
            , updatedSession |> Auth.authRefresh GotAuthRefreshResponse
            )

        ( GotAuthRefreshResponse response, _ ) ->
            ( toSession model
                |> updateSessionWithResult response
                |> updateModelWithSession model
            , Cmd.none
            )

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


updateModelWithSession : Model -> Session -> Model
updateModelWithSession model session =
    case model of
        Home home ->
            Home { home | session = session }

        Login login ->
            Login { login | session = session }

        Register register ->
            Register { register | session = session }


updateSessionWithResult : ResponseResult -> Session -> Session
updateSessionWithResult result session =
    case result of
        Ok ( _, res ) ->
            Response.stringToJson
                (Decode.succeed {})
                Response.decodeAuthResponse
                res
                |> updateSessionWithJson session

        Err _ ->
            session


updateSessionWithJson : Session -> JsonResponse {} AuthResponse -> Session
updateSessionWithJson session response =
    case response of
        JsonSuccess res ->
            Session.updateSessionVariant res session

        _ ->
            session



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Port.recieveSession RecieveSession



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Home home ->
            Page.viewPage GotHomeMsg (Home.view home)

        Login login ->
            Page.viewPage GotLoginMsg (Login.view login)

        Register register ->
            Page.viewPage GotRegisterMsg (Register.view register)



-- HELPERS


toSession : Model -> Session
toSession model =
    case model of
        Home home ->
            home.session

        Login login ->
            login.session

        Register register ->
            register.session


changeRouteTo : Url.Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        session =
            toSession model |> Session.updateSessionPath url
    in
    case Route.fromUrl url of
        Nothing ->
            ( model, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg
