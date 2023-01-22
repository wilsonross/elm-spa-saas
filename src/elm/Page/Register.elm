module Page.Register exposing (Model, Msg, init, view)

import Html exposing (Html, div, form, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder, bool, int, string)
import Json.Decode.Pipeline exposing (optional, required)
import Session exposing (Session)
import View
    exposing
        ( viewAuthLogo
        , viewButtonImage
        , viewCheckbox
        , viewEmailInput
        , viewInput
        , viewLink
        , viewPasswordInput
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
    { title = "Stamp | Register"
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
        , viewRegisterTitle
        , viewNameInput
        , viewEmailInput
        , viewPasswordInput
        , viewAdditional
        , viewLoginButton
        , viewAlternative
        ]


viewRegisterTitle : Html msg
viewRegisterTitle =
    viewTitle "Sign up"


viewNameInput : Html msg
viewNameInput =
    div [ class "flex gap-6 mb-6" ]
        [ viewInput [] "First name"
        , viewInput [] "Last name"
        ]


viewAdditional : Html msg
viewAdditional =
    div [ class "flex items-center justify-between mb-6" ]
        [ viewCheckbox [] True "remember" "Remember Me"
        ]


viewLoginButton : Html Msg
viewLoginButton =
    viewButtonImage [ class "w-full mb-4" ] NoOp "/static/img/signup.svg"


viewAlternative : Html msg
viewAlternative =
    div [ class "text-xs leading-[1.125rem] text-center" ]
        [ span [] [ text "Already have an account? " ]
        , viewLink [ class "text-turq" ] "/login" "Sign in"
        , span [] [ text " now" ]
        ]



-- UPDATE


type Msg
    = NoOp



-- JSON


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


type alias ErrorResponse =
    { code : Int
    , message : String
    , data : ErrorData
    }


type alias ErrorData =
    { email : Maybe ErrorMessage
    , password : Maybe ErrorMessage
    , passwordConfirm : Maybe ErrorMessage
    , firstName : Maybe ErrorMessage
    , lastName : Maybe ErrorMessage
    }


type alias ErrorMessage =
    { code : String
    , message : String
    }


decodeErrorResponse : Decoder ErrorResponse
decodeErrorResponse =
    Decode.succeed ErrorResponse
        |> required "code" int
        |> required "message" string
        |> required "data" decodeErrorData


decodeErrorData : Decoder ErrorData
decodeErrorData =
    Decode.succeed ErrorData
        |> optional "email" (Decode.map Just decodeErrorMessage) Nothing
        |> optional "password" (Decode.map Just decodeErrorMessage) Nothing
        |> optional "passwordConfirm" (Decode.map Just decodeErrorMessage) Nothing
        |> optional "firstName" (Decode.map Just decodeErrorMessage) Nothing
        |> optional "lastName" (Decode.map Just decodeErrorMessage) Nothing


decodeErrorMessage : Decoder ErrorMessage
decodeErrorMessage =
    Decode.succeed ErrorMessage
        |> required "code" string
        |> required "message" string
