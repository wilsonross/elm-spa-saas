module Page.Register exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, form, span, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Request
    exposing
        ( ErrorDetailed(..)
        , ErrorMessage
        , JsonResponse(..)
        , ResponseResult
        )
import Session exposing (Session, apiUrl, joinUrl)
import View
    exposing
        ( viewAuthLogo
        , viewButtonImage
        , viewCheckbox
        , viewEmailInput
        , viewErrors
        , viewInput
        , viewLink
        , viewPasswordInput
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , response : Status
    , email : String
    , password : String
    , passwordConfirm : String
    , firstName : String
    , lastName : String
    }


type Status
    = None
    | Failure
    | Loading
    | Response RegisterJsonResponse


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , response = None
      , email = ""
      , password = ""
      , passwordConfirm = ""
      , firstName = ""
      , lastName = ""
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRegisterResponse result ->
            handleGotRegisterResponse result model

        Register ->
            ( { model | response = Loading }
            , register model
            )

        EmailChanged email ->
            ( { model | email = email }, Cmd.none )

        PasswordChanged password ->
            ( { model
                | password = password
                , passwordConfirm = password
              }
            , Cmd.none
            )

        FirstNameChanged firstName ->
            ( { model | firstName = firstName }, Cmd.none )

        LastNameChanged lastName ->
            ( { model | lastName = lastName }, Cmd.none )


handleGotRegisterResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
handleGotRegisterResponse result model =
    case result of
        Ok ( _, res ) ->
            ( { model | response = Response (decodeJsonString res) }
            , Cmd.none
            )

        Err err ->
            handleErrorDetailed err model


handleErrorDetailed : ErrorDetailed -> Model -> ( Model, Cmd Msg )
handleErrorDetailed err model =
    case err of
        BadStatus _ res ->
            ( { model | response = Response (decodeJsonString res) }
            , Cmd.none
            )

        _ ->
            ( { model | response = Failure }, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Register"
    , content =
        div
            [ class <|
                "flex justify-center items-center h-screen rounded-md px-4"
            ]
            [ viewForm
            , fetchErrors model |> viewErrors
            ]
    }


viewForm : Html Msg
viewForm =
    form
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[3.125rem]"
                ++ " pb-[4.059rem] sm:px-10"
        ]
        [ viewAuthLogo
        , viewTitle "Sign Up"
        , viewNameInput
        , viewEmailInput EmailChanged
        , viewPasswordInput PasswordChanged
        , viewAdditional
        , viewLoginButton
        , viewAlternative
        ]


viewNameInput : Html Msg
viewNameInput =
    div [ class "flex gap-6 mb-6" ]
        [ viewInput [] "First name" FirstNameChanged
        , viewInput [] "Last name" LastNameChanged
        ]


viewAdditional : Html msg
viewAdditional =
    div [ class "flex items-center justify-between mb-6" ]
        [ viewCheckbox [] True "remember" "Remember Me"
        ]


viewLoginButton : Html Msg
viewLoginButton =
    viewButtonImage [ class "w-full mb-4" ] Register "/static/img/signup.svg"


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
        , body = Http.jsonBody (encodeForm model)
        , expect = Request.expectStringDetailed GotRegisterResponse
        }


encodeForm : Model -> Encode.Value
encodeForm model =
    Encode.object
        [ ( "email", Encode.string model.email )
        , ( "password", Encode.string model.password )
        , ( "passwordConfirm", Encode.string model.passwordConfirm )
        , ( "firstName", Encode.string model.firstName )
        , ( "lastName", Encode.string model.lastName )
        ]


fetchErrors : Model -> List ErrorMessage
fetchErrors model =
    case model.response of
        Failure ->
            [ Request.unknownError ]

        Response jsonResponse ->
            case jsonResponse of
                JsonError err ->
                    errorsToList [] err.data

                JsonSuccess _ ->
                    []

                JsonNone _ ->
                    [ Request.unknownError ]

        _ ->
            []


errorsToList : List ErrorMessage -> ErrorData -> List ErrorMessage
errorsToList list errorData =
    Request.prependMaybeError errorData.email list
        |> Request.prependMaybeError errorData.password
        |> Request.prependMaybeError errorData.passwordConfirm
        |> Request.prependMaybeError errorData.firstName
        |> Request.prependMaybeError errorData.lastName



-- JSON


type alias RegisterJsonResponse =
    JsonResponse ErrorData SuccessfulResponse


type alias SuccessfulResponse =
    { id : String
    , collectionId : String
    , collectionName : String
    , created : String
    , updated : String
    , username : String
    , verified : Bool
    , emailVisibility : Bool
    , email : String
    , firstName : String
    , lastName : String
    }


type alias ErrorData =
    { email : Maybe ErrorMessage
    , password : Maybe ErrorMessage
    , passwordConfirm : Maybe ErrorMessage
    , firstName : Maybe ErrorMessage
    , lastName : Maybe ErrorMessage
    }


decodeSuccessfulResponse : Decoder SuccessfulResponse
decodeSuccessfulResponse =
    Decode.succeed SuccessfulResponse
        |> required "id" string
        |> required "collectionId" string
        |> required "collectionName" string
        |> required "created" string
        |> required "updated" string
        |> required "username" string
        |> required "verified" bool
        |> required "emailVisibility" bool
        |> required "email" string
        |> required "firstName" string
        |> required "lastName" string


decodeErrorData : Decoder ErrorData
decodeErrorData =
    let
        decoder =
            Request.decodeErrorMessage
    in
    Decode.succeed ErrorData
        |> optional "email" (Decode.map Just decoder) Nothing
        |> optional "password" (Decode.map Just decoder) Nothing
        |> optional "passwordConfirm" (Decode.map Just decoder) Nothing
        |> optional "firstName" (Decode.map Just decoder) Nothing
        |> optional "lastName" (Decode.map Just decoder) Nothing


decodeJsonString : String -> RegisterJsonResponse
decodeJsonString jsonString =
    case Decode.decodeString decodeSuccessfulResponse jsonString of
        Ok res ->
            JsonSuccess res

        Err _ ->
            decodeJsonErrorString jsonString


decodeJsonErrorString : String -> RegisterJsonResponse
decodeJsonErrorString jsonString =
    let
        decoder =
            Request.decodeErrorResponse decodeErrorData
    in
    case Decode.decodeString decoder jsonString of
        Ok res ->
            JsonError res

        Err err ->
            JsonNone err
