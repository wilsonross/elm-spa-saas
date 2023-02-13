module Page.Register exposing (Model, Msg, init, update, view)

import Auth
import Browser.Navigation as Nav
import Html exposing (Html, div, form)
import Html.Attributes exposing (class, name, type_)
import Input
    exposing
        ( Input(..)
        , viewCheckbox
        , viewStatefulInput
        )
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Port
import Request exposing (Status(..), viewPreloader)
import Response
    exposing
        ( AuthJsonResponse
        , ErrorDetailed(..)
        , ErrorMessage
        , JsonResponse(..)
        , ResponseResult
        , UserResponse
        )
import Route
import Session exposing (Session)
import View
    exposing
        ( viewAlternative
        , viewAuthLogo
        , viewButtonImage
        , viewErrors
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , registerResponse : Status RegisterJsonResponse
    , loginResponse : Status AuthJsonResponse
    , email : Input
    , password : Input
    , passwordConfirm : Input
    , firstName : Input
    , lastName : Input
    , remember : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , registerResponse = None
      , loginResponse = None
      , email = Empty
      , password = Empty
      , passwordConfirm = Empty
      , firstName = Empty
      , lastName = Empty
      , remember = True
      }
    , Route.protected session True
    )



-- UPDATE


type Msg
    = GotRegisterResponse ResponseResult
    | GotLoginResponse ResponseResult
    | Register
    | Login
    | EmailChanged String
    | PasswordChanged String
    | FirstNameChanged String
    | LastNameChanged String
    | RememberChanged Bool
    | ResetErrorResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRegisterResponse res ->
            updateWithRegisterResponse res model

        GotLoginResponse res ->
            updateWithLoginResponse res model

        Register ->
            ( { model | registerResponse = Loading }
            , Auth.create
                GotRegisterResponse
                model.session
                model.email
                model.password
                model.passwordConfirm
                model.firstName
                model.lastName
            )

        Login ->
            ( { model | loginResponse = Loading }
            , Auth.authWithPassword
                GotLoginResponse
                model.session
                model.email
                model.password
            )

        EmailChanged email ->
            ( { model | email = Input.valueToInput email Input.checkEmail }
            , Cmd.none
            )

        PasswordChanged password ->
            ( { model
                | password = Input.valueToInput password checkPassword
                , passwordConfirm = Input.valueToInput password checkPassword
              }
            , Cmd.none
            )

        FirstNameChanged firstName ->
            ( { model | firstName = Input.valueToInput firstName (\_ -> True) }
            , Cmd.none
            )

        LastNameChanged lastName ->
            ( { model | lastName = Input.valueToInput lastName (\_ -> True) }
            , Cmd.none
            )

        RememberChanged remember ->
            ( { model | remember = remember }, Cmd.none )

        ResetErrorResponse ->
            ( { model | registerResponse = None }, Cmd.none )


updateWithRegisterResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
updateWithRegisterResponse result model =
    case result of
        Ok ( _, res ) ->
            updateWithLogin
                { model
                    | registerResponse = Response (stringToRegisterJson res)
                }

        Err err ->
            updateWithError err model


updateWithLoginResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
updateWithLoginResponse result model =
    case result of
        Ok ( _, res ) ->
            ( { model
                | loginResponse = Response (Response.stringToAuthJson res)
                , session =
                    Response.stringToAuthJson res
                        |> Session.updateSessionWithJson model.session
              }
            , cmdOnAuthSuccess (Response (Response.stringToAuthJson res)) model
            )

        Err err ->
            updateWithError err model


updateWithError : ErrorDetailed -> Model -> ( Model, Cmd Msg )
updateWithError err model =
    case err of
        BadStatus _ res ->
            ( let
                response =
                    Response (stringToRegisterJson res)
              in
              { model
                | registerResponse = response
                , email = responseToRegisterInput (\data -> data.email) model.email response
                , password = responseToRegisterInput (\data -> data.password) model.password response
                , firstName = responseToRegisterInput (\data -> data.firstName) model.firstName response
                , lastName = responseToRegisterInput (\data -> data.lastName) model.lastName response
              }
            , View.delay 2500 ResetErrorResponse
            )

        _ ->
            ( { model | registerResponse = Failure }
            , View.delay 2500 ResetErrorResponse
            )


updateWithLogin : Model -> ( Model, Cmd Msg )
updateWithLogin model =
    case model.registerResponse of
        Response jsonResponse ->
            case jsonResponse of
                JsonSuccess _ ->
                    update Login model

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


cmdOnAuthSuccess : Status AuthJsonResponse -> Model -> Cmd msg
cmdOnAuthSuccess status model =
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



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Register"
    , content =
        div
            [ class <|
                "flex justify-center items-center h-screen rounded-md px-4"
            ]
            [ viewForm model
            , viewErrors (errorsFromRegisterStatus model.registerResponse)
            , viewErrors (Response.errorsFromAuthStatus model.loginResponse)
            ]
    }


viewForm : Model -> Html Msg
viewForm model =
    form
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[3.125rem]"
                ++ " pb-[4.059rem] sm:px-10 rounded-md relative"
        ]
        [ viewAuthLogo
        , viewTitle "Sign Up"
        , viewNameInput model
        , viewStatefulInput
            model.email
            EmailChanged
            [ class "mb-6", type_ "email", name "email" ]
            "Email"
        , viewStatefulInput
            model.password
            PasswordChanged
            [ class "mb-6", type_ "password", name "password" ]
            "Password"
        , viewAdditional
        , viewRegisterButton
        , viewAlternative "Already have an account?" "Sign in" "now" "/login"
        , viewPreloader (isLoading model.registerResponse model.loginResponse)
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    div [ class "flex gap-6 mb-6" ]
        [ viewStatefulInput
            model.firstName
            FirstNameChanged
            [ type_ "text", name "firstName" ]
            "First Name"
        , viewStatefulInput
            model.lastName
            LastNameChanged
            [ type_ "text", name "lastName" ]
            "Last Name"
        ]


viewAdditional : Html Msg
viewAdditional =
    div [ class "flex items-center justify-between mb-6" ]
        [ viewCheckbox RememberChanged [] True "remember" "Remember Me"
        ]


viewRegisterButton : Html Msg
viewRegisterButton =
    viewButtonImage [ class "w-full mb-4" ] Register "/static/img/signup.svg"



-- HELPERS


checkPassword : String -> Bool
checkPassword password =
    String.length password >= 8 && String.length password <= 32


stringToRegisterJson : String -> RegisterJsonResponse
stringToRegisterJson str =
    Response.stringToJson
        decodeRegisterErrorData
        Response.decodeUserResponse
        str


isLoading : Status a -> Status b -> Status c
isLoading registerStatus loginStatus =
    if registerStatus == Loading || loginStatus == Loading then
        Loading

    else
        None



-- ERROR HELPERS


createRegisterErrorList : List ErrorMessage -> RegisterErrorData -> List ErrorMessage
createRegisterErrorList list errorData =
    Response.prependMaybeError errorData.email list
        |> Response.prependMaybeError errorData.password
        |> Response.prependMaybeError errorData.passwordConfirm
        |> Response.prependMaybeError errorData.firstName
        |> Response.prependMaybeError errorData.lastName


statusToMaybeRegisterError : Status RegisterJsonResponse -> Maybe RegisterErrorData
statusToMaybeRegisterError status =
    case status of
        Response jsonResponse ->
            case jsonResponse of
                JsonError err ->
                    Just err.data

                _ ->
                    Nothing

        _ ->
            Nothing


errorsFromRegisterStatus : Status RegisterJsonResponse -> List ErrorMessage
errorsFromRegisterStatus status =
    case status of
        Failure ->
            [ Response.unknownError ]

        Response jsonResponse ->
            case jsonResponse of
                JsonError err ->
                    createRegisterErrorList [] err.data

                JsonSuccess _ ->
                    []

                JsonNone _ ->
                    [ Response.unknownError ]

        _ ->
            []


responseToRegisterInput : (RegisterErrorData -> Maybe ErrorMessage) -> Input -> Status RegisterJsonResponse -> Input
responseToRegisterInput errToMessage currentInput status =
    Maybe.andThen errToMessage (statusToMaybeRegisterError status)
        |> Maybe.andThen (\_ -> Just (Input.invalidate currentInput))
        |> Maybe.withDefault currentInput



-- JSON


type alias RegisterJsonResponse =
    JsonResponse RegisterErrorData UserResponse


type alias RegisterErrorData =
    { email : Maybe ErrorMessage
    , password : Maybe ErrorMessage
    , passwordConfirm : Maybe ErrorMessage
    , firstName : Maybe ErrorMessage
    , lastName : Maybe ErrorMessage
    }


decodeRegisterErrorData : Decoder RegisterErrorData
decodeRegisterErrorData =
    let
        decoder =
            Response.decodeErrorMessage
    in
    Decode.succeed RegisterErrorData
        |> optional "email" (Decode.map Just decoder) Nothing
        |> optional "password" (Decode.map Just decoder) Nothing
        |> optional "passwordConfirm" (Decode.map Just decoder) Nothing
        |> optional "firstName" (Decode.map Just decoder) Nothing
        |> optional "lastName" (Decode.map Just decoder) Nothing
