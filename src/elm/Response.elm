module Response exposing
    ( AuthErrorData
    , AuthJsonResponse
    , AuthResponse
    , ErrorDetailed(..)
    , ErrorMessage
    , ErrorResponse
    , JsonResponse(..)
    , ResponseResult
    , UserResponse
    , decodeAuthErrorData
    , decodeAuthResponse
    , decodeErrorMessage
    , decodeErrorResponse
    , decodeUserResponse
    , errorsFromAuthStatus
    , expectStringDetailed
    , prependMaybeError
    , stringToAuthJson
    , stringToJson
    , unknownError
    )

import Http exposing (Expect, Metadata, Response)
import Json.Decode as Decode exposing (Decoder, Error, bool, int, string)
import Json.Decode.Pipeline exposing (optional, required)
import Request exposing (Status(..))



-- MODEL


type alias ResponseResult =
    Result ErrorDetailed ( Metadata, String )


type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String



-- HELPERS


expectStringDetailed : (ResponseResult -> msg) -> Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseString


convertResponseString : Response String -> ResponseResult
convertResponseString httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        Http.GoodStatus_ metadata body ->
            Ok ( metadata, body )



-- JSON


type alias UserResponse =
    { collectionId : String
    , collectionName : String
    , created : String
    , email : String
    , emailVisibility : Bool
    , firstName : String
    , id : String
    , lastName : String
    , updated : String
    , username : String
    , verified : Bool
    }


decodeUserResponse : Decoder UserResponse
decodeUserResponse =
    Decode.succeed UserResponse
        |> required "collectionId" string
        |> required "collectionName" string
        |> required "created" string
        |> optional "email" string ""
        |> required "emailVisibility" bool
        |> required "firstName" string
        |> required "id" string
        |> required "lastName" string
        |> required "updated" string
        |> required "username" string
        |> required "verified" bool


type JsonResponse errorData successfulResponse
    = JsonError (ErrorResponse errorData)
    | JsonSuccess successfulResponse
    | JsonNone Error


type alias ErrorResponse errorData =
    { code : Int
    , message : String
    , data : errorData
    }


decodeErrorResponse : Decoder errorData -> Decoder (ErrorResponse errorData)
decodeErrorResponse decodeErrorData =
    Decode.succeed ErrorResponse
        |> required "code" int
        |> required "message" string
        |> required "data" decodeErrorData


type alias ErrorMessage =
    { code : String
    , message : String
    }


decodeErrorMessage : Decoder ErrorMessage
decodeErrorMessage =
    Decode.succeed ErrorMessage
        |> required "code" string
        |> required "message" string


unknownError : ErrorMessage
unknownError =
    { code = "Unknown"
    , message = "An error occured"
    }


prependMaybeError : Maybe ErrorMessage -> List ErrorMessage -> List ErrorMessage
prependMaybeError maybe list =
    case maybe of
        Just a ->
            a :: list

        Nothing ->
            list


stringToJson : Decoder err -> Decoder success -> String -> JsonResponse err success
stringToJson decoderErr decoderSuccess jsonString =
    case Decode.decodeString decoderSuccess jsonString of
        Ok res ->
            JsonSuccess res

        Err _ ->
            stringToJson_ decoderErr jsonString


stringToJson_ : Decoder err -> String -> JsonResponse err success
stringToJson_ decodeErr jsonString =
    let
        decoder =
            decodeErrorResponse decodeErr
    in
    case Decode.decodeString decoder jsonString of
        Ok res ->
            JsonError res

        Err err ->
            JsonNone err


type alias AuthResponse =
    { record : UserResponse
    , token : String
    }


decodeAuthResponse : Decoder AuthResponse
decodeAuthResponse =
    Decode.succeed AuthResponse
        |> required "record" decodeUserResponse
        |> required "token" string


type alias AuthJsonResponse =
    JsonResponse AuthErrorData AuthResponse


type alias AuthErrorData =
    { identity : Maybe ErrorMessage
    , password : Maybe ErrorMessage
    }


decodeAuthErrorData : Decoder AuthErrorData
decodeAuthErrorData =
    let
        decoder =
            decodeErrorMessage
    in
    Decode.succeed AuthErrorData
        |> optional "identity" (Decode.map Just decoder) Nothing
        |> optional "password" (Decode.map Just decoder) Nothing


stringToAuthJson : String -> AuthJsonResponse
stringToAuthJson str =
    stringToJson
        decodeAuthErrorData
        decodeAuthResponse
        str


errorsFromAuthStatus : Status AuthJsonResponse -> List ErrorMessage
errorsFromAuthStatus status =
    case status of
        Failure ->
            [ unknownError ]

        Response jsonResponse ->
            case jsonResponse of
                JsonError err ->
                    createAuthErrorList [] err.data

                JsonSuccess _ ->
                    []

                JsonNone _ ->
                    [ unknownError ]

        _ ->
            []


createAuthErrorList : List ErrorMessage -> AuthErrorData -> List ErrorMessage
createAuthErrorList list errorData =
    prependMaybeError errorData.identity list
        |> prependMaybeError errorData.password
