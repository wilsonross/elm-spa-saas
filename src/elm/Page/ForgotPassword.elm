module Page.ForgotPassword exposing (Model, Msg, init, update, view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, name, type_)
import Input exposing (Input(..), viewStatefulInput)
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
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , email = Empty
      }
    , Route.protected session True
    )



-- UPDATE


type Msg
    = EmailChanged String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = Input.valueToInput email (\_ -> True) }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



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
    viewButtonImage [ class "w-full mb-4" ] NoOp "/static/img/reset.svg"
