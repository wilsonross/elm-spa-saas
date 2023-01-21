module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Page.Home as Home
import Page.Login as Login
import Page.Register as Register
import Route
import Session exposing (Flags, Session(..))
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
            Home.init (Session.init flags key url.path)
    in
    changeRouteTo
        url
        (Home home)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
            changeRouteTo
                url
                model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg

        ( GotLoginMsg _, Login login ) ->
            ( Login login, Cmd.none )

        ( GotRegisterMsg _, Register register ) ->
            ( Register register, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


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


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
