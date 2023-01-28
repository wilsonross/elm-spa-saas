module Page.Register exposing (Model, Msg, init, update, view)

import Html exposing (Attribute, Html, div, form, span, text)
import Html.Attributes exposing (class, disabled, name, type_)
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
        , viewErrors
        , viewInput
        , viewLink
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , response : Status
    , email : Input
    , password : Input
    , passwordConfirm : Input
    , firstName : Input
    , lastName : Input
    }


type Input
    = Empty
    | Invalid String
    | Valid String


type Status
    = None
    | Failure
    | Loading
    | Response RegisterJsonResponse


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
        GotRegisterResponse result ->
            handleGotRegisterResponse result model

        Register ->
            ( { model | response = Loading }
            , register model
            )

        EmailChanged email ->
            ( { model | email = invalidInput email checkEmail }, Cmd.none )

        PasswordChanged password ->
            ( { model
                | password = invalidInput password checkPassword
                , passwordConfirm = invalidInput password checkPassword
              }
            , Cmd.none
            )

        FirstNameChanged firstName ->
            ( { model | firstName = invalidInput firstName (\_ -> True) }
            , Cmd.none
            )

        LastNameChanged lastName ->
            ( { model | lastName = invalidInput lastName (\_ -> True) }
            , Cmd.none
            )

        ResetErrorResponse ->
            ( { model | response = None }, Cmd.none )


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
            , View.delay 2500 ResetErrorResponse
            )

        _ ->
            ( { model | response = Failure }
            , View.delay 2500 ResetErrorResponse
            )


submitActive : Model -> Bool
submitActive model =
    inputToBool model.firstName
        && inputToBool model.lastName
        && inputToBool model.email
        && inputToBool model.password
        && inputToBool model.passwordConfirm


inputToBool : Input -> Bool
inputToBool input =
    case input of
        Valid _ ->
            True

        _ ->
            False



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
            , fetchErrors model |> viewErrors
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
        , viewRegisterButton model
        , viewAlternative
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    div [ class "flex gap-6 mb-6" ]
        [ viewInput
            (name "firstName"
                :: (invalidBeforePost model.firstName
                        ++ (retrieveErrorData model |> invalidFirstName)
                   )
            )
            "First name"
            FirstNameChanged
        , viewInput
            (name "lastName"
                :: (invalidBeforePost model.lastName
                        ++ (retrieveErrorData model |> invalidLastName)
                   )
            )
            "Last name"
            LastNameChanged
        ]


viewEmailInput : Model -> (String -> msg) -> Html msg
viewEmailInput model msg =
    viewInput
        ([ class "mb-6", type_ "email", name "email" ]
            ++ invalidBeforePost model.email
            ++ (retrieveErrorData model |> invalidEmail)
        )
        "Email"
        msg


viewPasswordInput : Model -> (String -> msg) -> Html msg
viewPasswordInput model msg =
    viewInput
        ([ class "mb-6", type_ "password", name "password" ]
            ++ invalidBeforePost model.password
            ++ (retrieveErrorData model |> invalidPassword)
        )
        "Password"
        msg


viewAdditional : Html msg
viewAdditional =
    div [ class "flex items-center justify-between mb-6" ]
        [ viewCheckbox [] True "remember" "Remember Me"
        ]


viewRegisterButton : Model -> Html Msg
viewRegisterButton model =
    let
        disabledAttr =
            if submitActive model then
                []

            else
                [ disabled True ]
    in
    viewButtonImage
        (class "w-full mb-4" :: disabledAttr)
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
        , body = Http.jsonBody (encodeForm model)
        , expect = Request.expectStringDetailed GotRegisterResponse
        }


encodeForm : Model -> Encode.Value
encodeForm model =
    Encode.object
        [ ( "email"
          , Encode.string <| stringFromInput model.email
          )
        , ( "password"
          , Encode.string <| stringFromInput model.password
          )
        , ( "passwordConfirm"
          , Encode.string <| stringFromInput model.passwordConfirm
          )
        , ( "firstName"
          , Encode.string <| stringFromInput model.firstName
          )
        , ( "lastName"
          , Encode.string <| stringFromInput model.lastName
          )
        ]


stringFromInput : Input -> String
stringFromInput input =
    case input of
        Empty ->
            ""

        Invalid str ->
            str

        Valid str ->
            str


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


retrieveErrorData : Model -> Maybe ErrorData
retrieveErrorData model =
    case model.response of
        Response jsonResponse ->
            case jsonResponse of
                JsonError err ->
                    Just err.data

                _ ->
                    Nothing

        _ ->
            Nothing


maybeAttribute : Maybe a -> List (Attribute msg) -> List (Attribute msg)
maybeAttribute maybe attr =
    case maybe of
        Just _ ->
            attr

        Nothing ->
            []


invalidFirstName : Maybe ErrorData -> List (Attribute msg)
invalidFirstName errorData =
    case errorData of
        Just data ->
            maybeAttribute data.firstName [ class "border-red-500" ]

        Nothing ->
            []


invalidLastName : Maybe ErrorData -> List (Attribute msg)
invalidLastName errorData =
    case errorData of
        Just data ->
            maybeAttribute data.lastName [ class "border-red-500" ]

        Nothing ->
            []


invalidEmail : Maybe ErrorData -> List (Attribute msg)
invalidEmail errorData =
    case errorData of
        Just data ->
            maybeAttribute data.email [ class "border-red-500" ]

        Nothing ->
            []


invalidPassword : Maybe ErrorData -> List (Attribute msg)
invalidPassword errorData =
    case errorData of
        Just data ->
            maybeAttribute data.password [ class "border-red-500" ]

        Nothing ->
            []


invalidBeforePost : Input -> List (Attribute msg)
invalidBeforePost input =
    case input of
        Invalid _ ->
            [ class "border-red-500" ]

        _ ->
            []


invalidInput : String -> (String -> Bool) -> Input
invalidInput input check =
    case input of
        "" ->
            Empty

        _ ->
            if check input then
                Valid input

            else
                Invalid input


checkPassword : String -> Bool
checkPassword password =
    String.length password >= 8 && String.length password <= 32


checkEmail : String -> Bool
checkEmail email =
    String.contains "@" email && String.contains "." email



-- JSON


type alias RegisterJsonResponse =
    JsonResponse ErrorData SuccessfulResponse


type alias SuccessfulResponse =
    { collectionId : String
    , collectionName : String
    , created : String
    , emailVisibility : Bool
    , firstName : String
    , id : String
    , lastName : String
    , updated : String
    , username : String
    , verified : Bool
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
        |> required "collectionId" string
        |> required "collectionName" string
        |> required "created" string
        |> required "emailVisibility" bool
        |> required "firstName" string
        |> required "id" string
        |> required "lastName" string
        |> required "updated" string
        |> required "username" string
        |> required "verified" bool


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
