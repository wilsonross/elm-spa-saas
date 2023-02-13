module Page.ForgotPassword exposing (Model, Msg, init, update, view)

import Auth
import Html exposing (Html, div)
import Html.Attributes exposing (class, name, type_)
import Http exposing (Error)
import Input exposing (Input(..), viewStatefulInput)
import Request exposing (Status(..), viewPreloader)
import Response exposing (ErrorDetailed(..), JsonResponse(..))
import Route
import Session exposing (Session)
import View
    exposing
        ( viewAlternative
        , viewAuthLogo
        , viewButtonImage
        , viewDescription
        , viewRule
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , email : Input
    , response : Status ()
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
    | GotForgotPasswordResponse (Result Error ())
    | ResetPassword
    | ResetErrorResponse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = Input.valueToInput email Input.checkEmail }
            , Cmd.none
            )

        GotForgotPasswordResponse _ ->
            ( { model | response = Response () }, Cmd.none )

        ResetPassword ->
            shouldRequest model

        ResetErrorResponse ->
            ( { model | response = None }, Cmd.none )



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
        , viewFormOrSent model
        ]


viewFormOrSent : Model -> Html Msg
viewFormOrSent model =
    case model.response of
        Response _ ->
            viewSent

        _ ->
            viewForm model


viewForm : Model -> Html Msg
viewForm model =
    div
        []
        [ viewTitle "Forgot Password"
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
        , viewPreloader model.response
        ]


viewSent : Html msg
viewSent =
    div
        []
        [ viewRule
        , viewDescription "Reset request sent"
        , viewAlternative
            "Return to "
            "homepage"
            ""
            "/"
        ]


viewResetButton : Html Msg
viewResetButton =
    viewButtonImage
        [ class "w-full mb-4" ]
        ResetPassword
        "/static/img/reset.svg"



-- HELPERS


shouldRequest : Model -> ( Model, Cmd Msg )
shouldRequest model =
    case model.email of
        Valid _ ->
            ( { model | response = Loading }
            , Auth.requestPasswordReset
                GotForgotPasswordResponse
                model.session
                model.email
            )

        _ ->
            ( model, Cmd.none )
