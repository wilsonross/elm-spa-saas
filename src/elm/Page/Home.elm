module Page.Home exposing (Model, Msg, Status(..), init, update, view)

import Html exposing (Html, div, li, p, text, ul)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Session exposing (Session, apiUrl, joinUrl)
import View exposing (Link, viewFooter, viewHeader, viewLink)



-- MODEL


type alias Model =
    { session : Session
    , links : Status
    }


type Status
    = Failure
    | Loading
    | Success PaginatedResponse


init : Session -> ( Model, Cmd Msg )
init session =
    let
        url =
            apiUrl session
    in
    ( { session = session
      , links = Loading
      }
    , Http.get
        { url = joinUrl url "/api/collections/links/records"
        , expect = Http.expectJson GotPaginatedResponse decodePaginatedResponse
        }
    )



-- VIEW


view : Model -> { title : String, content : Html msg }
view model =
    let
        header =
            case model.links of
                Failure ->
                    text "Failed to load links"

                Loading ->
                    text "Loading links..."

                Success paginatedResponse ->
                    viewHeader (paginatedResponseToLinks paginatedResponse)
    in
    { title = "Stamp | Send Mail With Code"
    , content =
        div []
            [ header
            , div []
                [ ul []
                    [ li [] [ viewLink "/" "Home" ]
                    , li [] [ viewLink "/login" "Login" ]
                    , li [] [ viewLink "/register" "Register" ]
                    ]
                , p [] [ text "Home" ]
                ]
            , viewFooter
            ]
    }



-- UPDATE


type Msg
    = GotPaginatedResponse (Result Http.Error PaginatedResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPaginatedResponse result ->
            case result of
                Ok res ->
                    ( { model | links = Success res }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | links = Failure }
                    , Cmd.none
                    )



-- HELPERS


paginatedResponseToLinks : PaginatedResponse -> List Link
paginatedResponseToLinks response =
    List.map
        (\record ->
            { url = record.href
            , title = record.title
            }
        )
        response.items



-- JSON


type alias PaginatedResponse =
    { page : Int
    , perPage : Int
    , totalItems : Int
    , totalPages : Int
    , items :
        List CollectionResponse
    }


type alias CollectionResponse =
    { collectionId : String
    , collectionName : String
    , created : String
    , href : String
    , id : String
    , order : Int
    , title : String
    , updated : String
    }


decodePaginatedResponse : Decoder PaginatedResponse
decodePaginatedResponse =
    Decode.succeed PaginatedResponse
        |> required "page" int
        |> required "perPage" int
        |> required "totalItems" int
        |> required "totalPages" int
        |> custom (list decodeCollectionResponse)


decodeCollectionResponse : Decoder CollectionResponse
decodeCollectionResponse =
    Decode.succeed CollectionResponse
        |> required "collectionId" string
        |> required "collectionName" string
        |> required "created" string
        |> required "href" string
        |> required "id" string
        |> optional "order" int 0
        |> required "title" string
        |> required "updated" string
