module Page.Login exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, form)
import Html.Attributes exposing (class, name, type_)
import Http
import Input exposing (Input(..), viewCheckbox, viewStatefulInput)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (optional, required)
import Port
import Request exposing (Status(..))
import Response
    exposing
        ( ErrorDetailed(..)
        , ErrorMessage
        , JsonResponse(..)
        , ResponseResult
        , UserResponse
        )
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
    , response : Status LoginJsonResponse
    , identity : Input
    , password : Input
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , response = None
      , identity = Empty
      , password = Empty
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotLoginResponse ResponseResult
    | Login
    | IdentityChanged String
    | PasswordChanged String
    | ResetErrorResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLoginResponse res ->
            updateWithResponse res model

        Login ->
            ( { model | response = Loading }
            , login model
            )

        IdentityChanged identity ->
            ( { model | identity = Input.valueToInput identity (\_ -> True) }
            , Cmd.none
            )

        PasswordChanged password ->
            ( { model | password = Input.valueToInput password (\_ -> True) }
            , Cmd.none
            )

        ResetErrorResponse ->
            ( { model | response = None }, Cmd.none )


updateWithResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
updateWithResponse result model =
    case result of
        Ok ( _, res ) ->
            ( { model | response = Response (stringToJson res) }
            , Port.setCookie
                ( "session"
                , Response (stringToJson res)
                    |> responseToToken
                , 30
                )
            )

        Err err ->
            updateWithError err model


updateWithError : ErrorDetailed -> Model -> ( Model, Cmd Msg )
updateWithError err model =
    case err of
        BadStatus _ res ->
            ( let
                response =
                    Response (stringToJson res)
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



-- HELPERS


login : Model -> Cmd Msg
login model =
    Http.post
        { url =
            Request.joinUrl
                (Session.apiUrl model.session)
                "/api/collections/users/auth-with-password"
        , body =
            Http.jsonBody
                (Input.encodeInput
                    [ ( "identity", model.identity )
                    , ( "password", model.password )
                    ]
                )
        , expect = Response.expectStringDetailed GotLoginResponse
        }


stringToJson : String -> LoginJsonResponse
stringToJson str =
    Response.stringToJson
        decodeErrorData
        decodeSuccessfulResponse
        str



-- ERROR HELPERS


createErrorList : List ErrorMessage -> ErrorData -> List ErrorMessage
createErrorList list errorData =
    Response.prependMaybeError errorData.identity list
        |> Response.prependMaybeError errorData.password


statusToMaybeError : Status LoginJsonResponse -> Maybe ErrorData
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


errorsFromStatus : Status LoginJsonResponse -> List ErrorMessage
errorsFromStatus status =
    case status of
        Failure ->
            [ Response.unknownError ]

        Response jsonResponse ->
            case jsonResponse of
                JsonError err ->
                    createErrorList [] err.data

                JsonSuccess _ ->
                    []

                JsonNone _ ->
                    [ Response.unknownError ]

        _ ->
            []


responseToInput : (ErrorData -> Maybe ErrorMessage) -> Input -> Status LoginJsonResponse -> Input
responseToInput errToMessage currentInput status =
    case statusToMaybeError status of
        Just err ->
            case errToMessage err of
                Just _ ->
                    Invalid (Input.stringFromInput currentInput)

                Nothing ->
                    currentInput

        Nothing ->
            currentInput


responseToToken : Status LoginJsonResponse -> String
responseToToken status =
    case status of
        Response jsonResponse ->
            case jsonResponse of
                JsonSuccess res ->
                    res.token

                _ ->
                    ""

        _ ->
            ""



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
            , errorsFromStatus model.response |> viewErrors
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


viewAdditional : Html msg
viewAdditional =
    div [ class "flex items-center justify-between mb-6" ]
        [ viewCheckbox [] True "remember" "Remember Me"
        , viewLink [ class "text-xs leading-[1.125rem]" ]
            "/forgot-password"
            "Forgot Password?"
        ]


viewLoginButton : Html Msg
viewLoginButton =
    viewButtonImage [ class "w-full mb-4" ] Login "/static/img/signin.svg"



-- JSON


type alias LoginJsonResponse =
    JsonResponse ErrorData SuccessfulResponse


type alias SuccessfulResponse =
    { record : UserResponse
    , token : String
    }


type alias ErrorData =
    { identity : Maybe ErrorMessage
    , password : Maybe ErrorMessage
    }


decodeSuccessfulResponse : Decoder SuccessfulResponse
decodeSuccessfulResponse =
    Decode.succeed SuccessfulResponse
        |> required "record" Response.decodeUserResponse
        |> required "token" string


decodeErrorData : Decoder ErrorData
decodeErrorData =
    let
        decoder =
            Response.decodeErrorMessage
    in
    Decode.succeed ErrorData
        |> optional "identity" (Decode.map Just decoder) Nothing
        |> optional "password" (Decode.map Just decoder) Nothing
