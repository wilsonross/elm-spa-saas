module Page.Register exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, form, span, text)
import Html.Attributes exposing (class, disabled, name, type_)
import Http
import Input exposing (Input(..), viewCheckbox, viewInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Request exposing (Status(..))
import Response
    exposing
        ( ErrorDetailed(..)
        , ErrorMessage
        , JsonResponse(..)
        , ResponseResult
        , UserResponse
        )
import Session exposing (Session, apiUrl, joinUrl)
import View
    exposing
        ( viewAuthLogo
        , viewButtonImage
        , viewErrors
        , viewLink
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , response : Status RegisterJsonResponse
    , email : Input
    , password : Input
    , passwordConfirm : Input
    , firstName : Input
    , lastName : Input
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , response = None
      , email = Empty
      , password = Empty
      , passwordConfirm = Empty
      , firstName = Empty
      , lastName = Empty
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotRegisterResponse ResponseResult
    | Register
    | EmailChanged String
    | PasswordChanged String
    | FirstNameChanged String
    | LastNameChanged String
    | ResetErrorResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRegisterResponse res ->
            updateWithResponse res model

        Register ->
            ( { model | response = Loading }
            , register model
            )

        EmailChanged email ->
            ( { model | email = Input.valueToInput email checkEmail }
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

        ResetErrorResponse ->
            ( { model | response = None }, Cmd.none )


updateWithResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
updateWithResponse result model =
    case result of
        Ok ( _, res ) ->
            ( { model | response = Response (stringToJson res) }
            , Cmd.none
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
                , email = responseToInput (\data -> data.email) model.email response
                , password = responseToInput (\data -> data.password) model.password response
                , firstName = responseToInput (\data -> data.firstName) model.firstName response
                , lastName = responseToInput (\data -> data.lastName) model.lastName response
              }
            , View.delay 2500 ResetErrorResponse
            )

        _ ->
            ( { model | response = Failure }
            , View.delay 2500 ResetErrorResponse
            )



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
        , viewTitle "Sign Up"
        , viewNameInput model
        , viewEmailInput model EmailChanged
        , viewPasswordInput model PasswordChanged
        , viewAdditional
        , viewRegisterButton
        , viewAlternative
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    div [ class "flex gap-6 mb-6" ]
        [ viewInput
            [ name "firstName", Input.inputBorder model.firstName ]
            "First name"
            FirstNameChanged
        , viewInput
            [ name "lastName", Input.inputBorder model.lastName ]
            "Last name"
            LastNameChanged
        ]


viewEmailInput : Model -> (String -> msg) -> Html msg
viewEmailInput model msg =
    viewInput
        [ class "mb-6"
        , type_ "email"
        , name "email"
        , Input.inputBorder model.email
        ]
        "Email"
        msg


viewPasswordInput : Model -> (String -> msg) -> Html msg
viewPasswordInput model msg =
    viewInput
        [ class "mb-6"
        , type_ "password"
        , name "password"
        , Input.inputBorder model.password
        ]
        "Password"
        msg


viewAdditional : Html msg
viewAdditional =
    div [ class "flex items-center justify-between mb-6" ]
        [ viewCheckbox [] True "remember" "Remember Me"
        ]


viewRegisterButton : Html Msg
viewRegisterButton =
    viewButtonImage
        [ class "w-full mb-4" ]
        Register
        "/static/img/signup.svg"


viewAlternative : Html msg
viewAlternative =
    div [ class "text-xs leading-[1.125rem] text-center" ]
        [ span [] [ text "Already have an account? " ]
        , viewLink [ class "text-turq" ] "/login" "Sign in"
        , span [] [ text " now" ]
        ]



-- HELPERS


register : Model -> Cmd Msg
register model =
    Http.post
        { url = joinUrl (apiUrl model.session) "/api/collections/users/records"
        , body =
            Http.jsonBody
                (Input.encodeInput
                    [ ( "email", model.email )
                    , ( "password", model.password )
                    , ( "passwordConfirm", model.passwordConfirm )
                    , ( "firstName", model.firstName )
                    , ( "lastName", model.lastName )
                    ]
                )
        , expect = Response.expectStringDetailed GotRegisterResponse
        }


checkPassword : String -> Bool
checkPassword password =
    String.length password >= 8 && String.length password <= 32


checkEmail : String -> Bool
checkEmail email =
    String.contains "@" email && String.contains "." email


stringToJson : String -> RegisterJsonResponse
stringToJson str =
    Response.stringToJson
        decodeErrorData
        Response.decodeUserResponse
        str



-- ERROR HELPERS


createErrorList : List ErrorMessage -> ErrorData -> List ErrorMessage
createErrorList list errorData =
    Response.prependMaybeError errorData.email list
        |> Response.prependMaybeError errorData.password
        |> Response.prependMaybeError errorData.passwordConfirm
        |> Response.prependMaybeError errorData.firstName
        |> Response.prependMaybeError errorData.lastName


statusToMaybeError : Status RegisterJsonResponse -> Maybe ErrorData
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


errorsFromStatus : Status RegisterJsonResponse -> List ErrorMessage
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


responseToInput : (ErrorData -> Maybe ErrorMessage) -> Input -> Status RegisterJsonResponse -> Input
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



-- JSON


type alias RegisterJsonResponse =
    JsonResponse ErrorData UserResponse


type alias ErrorData =
    { email : Maybe ErrorMessage
    , password : Maybe ErrorMessage
    , passwordConfirm : Maybe ErrorMessage
    , firstName : Maybe ErrorMessage
    , lastName : Maybe ErrorMessage
    }


decodeErrorData : Decoder ErrorData
decodeErrorData =
    let
        decoder =
            Response.decodeErrorMessage
    in
    Decode.succeed ErrorData
        |> optional "email" (Decode.map Just decoder) Nothing
        |> optional "password" (Decode.map Just decoder) Nothing
        |> optional "passwordConfirm" (Decode.map Just decoder) Nothing
        |> optional "firstName" (Decode.map Just decoder) Nothing
        |> optional "lastName" (Decode.map Just decoder) Nothing
