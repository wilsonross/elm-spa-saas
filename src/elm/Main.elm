module Main exposing (main)

import Auth
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Page.Error as Error
import Page.ForgotPassword as ForgotPassword
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Port
import Response
    exposing
        ( JsonResponse(..)
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
    | Error Error.Model
    | ForgotPassword ForgotPassword.Model


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( home, _ ) =
            Home.init (Session.initLoading flags key url.path)
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
    | GotErrorMsg Error.Msg
    | GotForgotPasswordMsg ForgotPassword.Msg


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
            handleRecieveSession cookie model

        ( GotAuthRefreshResponse response, _ ) ->
            handleAuthRefreshResponse response model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg

        ( GotErrorMsg subMsg, Error error ) ->
            Error.update subMsg error
                |> updateWith Error GotErrorMsg

        ( GotForgotPasswordMsg subMsg, ForgotPassword forgotPassword ) ->
            ForgotPassword.update subMsg forgotPassword
                |> updateWith ForgotPassword GotForgotPasswordMsg

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

        Error error ->
            Error { error | session = session }

        ForgotPassword forgotPassword ->
            ForgotPassword { forgotPassword | session = session }


updateSessionWithResult : ResponseResult -> Session -> Session
updateSessionWithResult result session =
    case result of
        Ok ( _, res ) ->
            Response.stringToAuthJson res
                |> Session.updateSessionWithJson session

        Err _ ->
            Session.forceGuestSession session



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

        Error error ->
            Page.viewPage GotErrorMsg (Error.view error)

        ForgotPassword forgotPassword ->
            Page.viewPage GotForgotPasswordMsg (ForgotPassword.view forgotPassword)



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

        Error error ->
            error.session

        ForgotPassword forgotPassword ->
            forgotPassword.session


changeRouteTo : Url.Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        session =
            toSession model
                |> Session.updateSessionPath url
    in
    case Route.fromUrl url of
        Nothing ->
            Error.init session 404 "Page not found"
                |> updateWith Error GotErrorMsg

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg

        Just Route.ForgotPassword ->
            ForgotPassword.init session
                |> updateWith ForgotPassword GotForgotPasswordMsg

        Just Route.Account ->
            underConstruction session

        Just Route.About ->
            underConstruction session

        Just Route.HowItWorks ->
            underConstruction session

        Just Route.Testimonials ->
            underConstruction session

        Just Route.Contact ->
            underConstruction session

        Just Route.Pricing ->
            underConstruction session


resetModel : Model -> Cmd Msg -> ( Model, Cmd Msg )
resetModel model msg =
    case model of
        Home home ->
            Home.init home.session
                |> updateWith Home GotHomeMsg
                |> addCmdMsg msg

        Login login ->
            Login.init login.session
                |> updateWith Login GotLoginMsg
                |> addCmdMsg msg

        Register register ->
            Register.init register.session
                |> updateWith Register GotRegisterMsg
                |> addCmdMsg msg

        Error error ->
            Error.init error.session 404 "Page not found"
                |> updateWith Error GotErrorMsg
                |> addCmdMsg msg

        ForgotPassword forgotPassword ->
            ForgotPassword.init forgotPassword.session
                |> updateWith ForgotPassword GotForgotPasswordMsg
                |> addCmdMsg msg


addCmdMsg : Cmd Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addCmdMsg newCmd ( model, cmd ) =
    ( model, Cmd.batch [ cmd, newCmd ] )


underConstruction : Session -> ( Model, Cmd Msg )
underConstruction session =
    Error.init session 503 "Under construction"
        |> updateWith Error GotErrorMsg


handleRecieveSession : Cookie -> Model -> ( Model, Cmd Msg )
handleRecieveSession ( key, value, expiry ) model =
    let
        updatedSession =
            toSession model
                |> Session.updateSessionWithCookie
                    ( key, value, expiry )
    in
    if String.length value == 0 then
        resetModel
            (Session.forceGuestSession updatedSession
                |> updateModelWithSession model
            )
            Cmd.none

    else
        updatedSession
            |> updateModelWithSession model
            |> authRefresh


authRefresh : Model -> ( Model, Cmd Msg )
authRefresh model =
    ( model
    , toSession model
        |> Auth.authRefresh GotAuthRefreshResponse
    )


handleAuthRefreshResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
handleAuthRefreshResponse response model =
    resetModel
        (toSession model
            |> updateSessionWithResult response
            |> updateModelWithSession model
        )
        Cmd.none
