module Page.Search exposing (Model, Msg, init, update, view)

import Header exposing (Status)
import Html exposing (Html, a, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Page exposing (viewComponent)
import Request exposing (Status(..))
import Response exposing (PaginatedResponse)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    , response : Status SearchResponse
    , query : String
    }


type alias SearchResponse =
    PaginatedResponse (List CollectionResponse)


init : Session -> String -> ( Model, Cmd Msg )
init session query =
    let
        ( header, subMsg ) =
            Header.init session
    in
    ( { session = session
      , header = header
      , response = Loading
      , query = query
      }
    , Cmd.batch
        [ Cmd.map GotHeaderMsg subMsg
        , Http.get
            { url =
                Request.joinUrl
                    (Session.apiUrl session)
                    (searchBuilder query)
            , expect =
                Http.expectJson
                    GotResponse
                    (Response.decodePaginatedResponse
                        decodeCollectionResponse
                    )
            }
        ]
    )



-- UPDATE


type Msg
    = GotHeaderMsg Header.Msg
    | GotResponse (Result Http.Error SearchResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHeaderMsg subMsg ->
            Header.update subMsg model.header
                |> updateWith model GotHeaderMsg

        GotResponse result ->
            case result of
                Ok res ->
                    updateModelOnSuccessResponse model res

                Err _ ->
                    ( { model | response = Failure }
                    , Cmd.none
                    )


updateModelOnSuccessResponse : Model -> SearchResponse -> ( Model, Cmd msg )
updateModelOnSuccessResponse model response =
    ( { model
        | response = Request.Response response
      }
    , Cmd.none
    )


updateWith : Model -> (subMsg -> Msg) -> ( Header.Model, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith model toMsg ( subModel, subCmd ) =
    ( { model | header = subModel }
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Results"
    , content =
        div [ class "h-full" ]
            [ viewHeader model
            , div
                [ class <|
                    "px-6 sm:px-12 lg:px-5 w-full mx-auto max-w-[76.5rem] pt-5"
                        ++ " lg:pt-0 mt-6 md:mt-28 mb-20"
                ]
                [ viewSearchResults model
                ]
            ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    Header.view model.header
        |> viewComponent GotHeaderMsg


viewSearchResults : Model -> Html msg
viewSearchResults model =
    div
        []
        [ viewTitle model.query
        , viewSearchItems
        ]


viewTitle : String -> Html msg
viewTitle query =
    h1
        [ class "text-xl text-center font-medium" ]
        [ capitalize query
            |> addQuotes
            |> text
        ]


viewSearchItems : Html msg
viewSearchItems =
    div
        []
        []


viewSearchItem : String -> String -> String -> String -> Html msg
viewSearchItem href src title tagline =
    a
        [ class "p-[1.125rem] block w-full flex rounded-md" ]
        [ viewSearchItemImage src ]


viewSearchItemImage : String -> Html msg
viewSearchItemImage url =
    if url == "" then
        div
            [ class "w-16 h-16 shrink-0 bg-grey-1" ]
            []

    else
        img
            [ src url, class "w-16 h-16 shrink-0" ]
            []



-- HELPER


addQuotes : String -> String
addQuotes str =
    "‘" ++ str ++ "’"


capitalize : String -> String
capitalize str =
    String.toUpper
        (String.left 1 str)
        ++ String.dropLeft 1 str


searchBuilder : String -> String
searchBuilder query =
    "/api/collections/cms/records?filter=(title~%'"
        ++ query
        ++ "%'||tagline~%'"
        ++ query
        ++ "%'||description~%'"
        ++ query
        ++ "%')"



-- JSON


type alias CollectionResponse =
    { collectionId : String
    , collectionName : String
    , content : String
    , created : String
    , id : String
    , image : String
    , tagline : String
    , title : String
    , updated : String
    }


decodeCollectionResponse : Decoder CollectionResponse
decodeCollectionResponse =
    Decode.succeed CollectionResponse
        |> required "collectionId" string
        |> required "collectionName" string
        |> required "content" string
        |> required "created" string
        |> required "id" string
        |> required "image" string
        |> required "tagline" string
        |> required "title" string
        |> required "updated" string
