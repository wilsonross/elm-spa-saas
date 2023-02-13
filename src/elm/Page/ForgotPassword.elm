module Page.ForgotPassword exposing (Model, Msg, init, update, view)

import Auth
import Html exposing (Html, div)
import Html.Attributes exposing (class, name, type_)
import Input exposing (Input(..), viewStatefulInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Request exposing (Status(..))
import Response
    exposing
        ( ErrorDetailed(..)
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
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , email : Input
    , response : Status ForgotJsonResponse
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , email = Empty
      , response = None
      }
    , Route.protected session True
    )



-- UPDATE


type Msg
    = EmailChanged String
    | GotForgotPasswordResponse ResponseResult
    | ResetPassword
    | ResetErrorResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = Input.valueToInput email (\_ -> True) }
            , Cmd.none
            )

        GotForgotPasswordResponse res ->
            updateWithResponse res model

        ResetPassword ->
            ( { model | response = Loading }
            , Auth.requestPasswordReset
                GotForgotPasswordResponse
                model.session
                model.email
            )

        ResetErrorResponse ->
            ( { model | response = None }, Cmd.none )


updateWithResponse : ResponseResult -> Model -> ( Model, Cmd Msg )
updateWithResponse result model =
    case result of
        Ok ( _, res ) ->
            ( { model
                | response = Response (stringToForgotJson res)
              }
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
                    Response (stringToForgotJson res)
              in
              { model
                | response = response
                , email =
                    responseToInput
                        (\data -> data.email)
                        model.email
                        response
              }
            , View.delay 2500 ResetErrorResponse
            )

        _ ->
            ( { model | response = Failure }
            , View.delay 2500 ResetErrorResponse
            )



-- ERROR HELPERS


statusToMaybeError : Status ForgotJsonResponse -> Maybe ErrorData
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


responseToInput : (ErrorData -> Maybe ErrorMessage) -> Input -> Status ForgotJsonResponse -> Input
responseToInput errToMessage currentInput status =
    Maybe.andThen errToMessage (statusToMaybeError status)
        |> Maybe.andThen (\_ -> Just (Input.invalidate currentInput))
        |> Maybe.withDefault (Input.invalidate currentInput)



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Forgot Password"
    , content =
        div
            [ class <|
                "flex justify-center items-center h-screen rounded-md px-4"
            ]
            [ viewModal model
            ]
    }


viewModal : Model -> Html Msg
viewModal model =
    div
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[3.125rem]"
                ++ " pb-[4.059rem] sm:px-10 rounded-md relative"
        ]
        [ viewAuthLogo
        , viewTitle "Forgot Password"
        , viewStatefulInput
            model.email
            EmailChanged
            [ class "mb-6", type_ "email", name "email" ]
            "Email"
        , viewResetButton
        , viewAlternative
            "Not what you're looking for? "
            "Sign in"
            "now"
            "/login"
        ]


viewResetButton : Html Msg
viewResetButton =
    viewButtonImage
        [ class "w-full mb-4" ]
        ResetPassword
        "/static/img/reset.svg"



-- JSON


type alias ForgotJsonResponse =
    JsonResponse ErrorData {}


type alias ErrorData =
    { email : Maybe ErrorMessage
    }


stringToForgotJson : String -> ForgotJsonResponse
stringToForgotJson str =
    Response.stringToJson
        decodeErrorData
        decodeSuccess
        str


decodeSuccess : Decoder {}
decodeSuccess =
    Decode.succeed {}


decodeErrorData : Decoder ErrorData
decodeErrorData =
    Decode.succeed ErrorData
        |> optional "email" (Decode.map Just Response.decodeErrorMessage) Nothing
