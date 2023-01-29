module Page.Login exposing (Model, Msg, init, view)

import Html exposing (Html, div, form, span, text)
import Html.Attributes exposing (class, name, type_)
import Input exposing (viewCheckbox, viewInput)
import Session exposing (Session)
import View
    exposing
        ( viewAuthLogo
        , viewButtonImage
        , viewLink
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view _ =
    { title = "Stamp | Login"
    , content =
        div
            [ class <|
                "flex justify-center items-center h-screen rounded-md px-4"
            ]
            [ viewForm ]
    }


viewForm : Html Msg
viewForm =
    form
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[3.125rem]"
                ++ " pb-[4.059rem] sm:px-10"
        ]
        [ viewAuthLogo
        , viewTitle "Log in"
        , viewEmailInput EmailChanged
        , viewPasswordInput PasswordChanged
        , viewAdditional
        , viewLoginButton
        , viewAlternative
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


viewAlternative : Html msg
viewAlternative =
    div [ class "text-xs leading-[1.125rem] text-center" ]
        [ span [] [ text "Don't have an account? " ]
        , viewLink [ class "text-turq" ] "/register" "Sign up"
        , span [] [ text " now" ]
        ]


viewEmailInput : (String -> msg) -> Html msg
viewEmailInput msg =
    viewInput [ class "mb-6", type_ "email", name "email" ] "Email" msg


viewPasswordInput : (String -> msg) -> Html msg
viewPasswordInput msg =
    viewInput
        [ class "mb-6", type_ "password", name "password" ]
        "Password"
        msg



-- UPDATE


type Msg
    = Login
    | EmailChanged String
    | PasswordChanged String
