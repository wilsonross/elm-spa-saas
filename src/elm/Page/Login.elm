module Page.Login exposing (Model, Msg, init, update, view)

import Auth
import Browser.Navigation as Nav
import Html exposing (Html, div, form)
import Html.Attributes exposing (class, name, type_)
import Input exposing (Input(..), viewCheckbox, viewStatefulInput)
import Port
import Request exposing (Status(..))
import Response
    exposing
        ( AuthErrorData
        , AuthJsonResponse
        , ErrorDetailed(..)
        , ErrorMessage
        , JsonResponse(..)
        , ResponseResult
        )
import Route
import Session exposing (Session)
import View
    exposing
        ( viewAlternative
        , viewAuthLogo
        , viewButtonImage
        , viewErrors
        , viewLink
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , response : Status AuthJsonResponse
    , identity : Input
    , password : Input
    , remember : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , response = None
      , identity = Empty
      , password = Empty
      , remember = True
      }
    , Route.protected session True
    )



-- UPDATE


type Msg
    = GotLoginResponse ResponseResult
    | Login
    | IdentityChanged String
    | PasswordChanged String
    | RememberChanged Bool
    | ResetErrorResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLoginResponse res ->
            updateWithResponse res model

        Login ->
            ( { model | response = Loading }
            , Auth.authWithPassword
                GotLoginResponse
                model.session
                model.identity
                model.password
            )

        IdentityChanged identity ->
            ( { model | identity = Input.valueToInput identity (\_ -> True) }
            , Cmd.none
            )

        PasswordChanged password ->
            ( { model | password = Input.valueToInput password (\_ -> True) }
            , Cmd.none
            )

        RememberChanged remember ->
            ( { model | remember = remember }, Cmd.none )

        ResetErrorResponse ->
            ( { model | response = None }, Cmd.none )


updateWithResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
updateWithResponse result model =
    case result of
        Ok ( _, res ) ->
            ( { model
                | response = Response (Response.stringToAuthJson res)
                , session =
                    Session.updateSessionWithJson
                        model.session
                        (Response.stringToAuthJson res)
              }
            , cmdOnSuccess (Response (Response.stringToAuthJson res)) model
            )

        Err err ->
            updateWithError err model


cmdOnSuccess : Status AuthJsonResponse -> Model -> Cmd msg
cmdOnSuccess status model =
    case status of
        Response jsonResponse ->
            case jsonResponse of
                JsonSuccess res ->
                    Cmd.batch
                        [ Port.setSession
                            ( res.token, Session.rememberMe model.remember )
                        , Nav.pushUrl
                            (Session.navKey model.session)
                            "/account"
                        ]

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


updateWithError : ErrorDetailed -> Model -> ( Model, Cmd Msg )
updateWithError err model =
    case err of
        BadStatus _ res ->
            ( let
                response =
                    Response (Response.stringToAuthJson res)
              in
              { model
                | response = response
                , identity = responseToInput (\data -> data.identity) model.identity response
                , password = responseToInput (\data -> data.password) model.password response
              }
            , View.delay 2500 ResetErrorResponse
            )

        _ ->
            ( { model | response = Failure }
            , View.delay 2500 ResetErrorResponse
            )



-- ERROR HELPERS


statusToMaybeError : Status AuthJsonResponse -> Maybe AuthErrorData
statusToMaybeError status =
    case status of
        Response jsonResponse ->
            case jsonResponse of
                JsonError err ->
                    Just err.data

                _ ->
                    Nothing

        _ ->
            Nothing


responseToInput : (AuthErrorData -> Maybe ErrorMessage) -> Input -> Status AuthJsonResponse -> Input
responseToInput errToMessage currentInput status =
    Maybe.andThen errToMessage (statusToMaybeError status)
        |> Maybe.andThen (\_ -> Just (Input.invalidate currentInput))
        |> Maybe.withDefault currentInput



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Login"
    , content =
        div
            [ class <|
                "flex justify-center items-center h-screen rounded-md px-4"
            ]
            [ viewForm model
            , Response.errorsFromAuthStatus model.response |> viewErrors
            ]
    }


viewForm : Model -> Html Msg
viewForm model =
    form
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[3.125rem]"
                ++ " pb-[4.059rem] sm:px-10"
        ]
        [ viewAuthLogo
        , viewTitle "Log in"
        , viewStatefulInput
            model.identity
            IdentityChanged
            [ class "mb-6", type_ "email", name "email" ]
            "Email"
        , viewStatefulInput model.password
            PasswordChanged
            [ class "mb-6", type_ "password", name "password" ]
            "Password"
        , viewAdditional
        , viewLoginButton
        , viewAlternative "Don't have an account?" "Sign up" "now" "/register"
        ]


viewAdditional : Html Msg
viewAdditional =
    div [ class "flex items-center justify-between mb-6" ]
        [ viewCheckbox
            RememberChanged
            []
            True
            "remember"
            "Remember Me"
        , viewLink
            [ class "text-xs leading-[1.125rem]" ]
            "/forgot-password"
            "Forgot Password?"
        ]


viewLoginButton : Html Msg
viewLoginButton =
    viewButtonImage [ class "w-full mb-4" ] Login "/static/img/signin.svg"
