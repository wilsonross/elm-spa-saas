module Request exposing
    ( ErrorDetailed(..)
    , ErrorMessage
    , ErrorResponse
    , ResponseResult
    , decodeErrorMessage
    , decodeErrorResponse
    , expectStringDetailed
    )

import Http exposing (Expect, Metadata, Response)
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)


type alias ResponseResult =
    Result ErrorDetailed ( Metadata, String )


type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String


expectStringDetailed : (ResponseResult -> msg) -> Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseString


convertResponseString : Response String -> Result ErrorDetailed ( Metadata, String )
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



-- JSON ERROR


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
