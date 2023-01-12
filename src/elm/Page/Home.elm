module Page.Home exposing (Model, Msg, Status(..), init, update, view)

import Html exposing (Html, div, li, p, text, ul)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, at, int, list, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Session exposing (Session, apiUrl, joinUrl)
import View exposing (Link, viewButtonImage, viewFooter, viewHeader, viewLink)



-- MODEL


type alias Model =
    { session : Session
    , links : Status
    , navOpen : Bool
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
      , navOpen = False
      }
    , Http.get
        { url = joinUrl url "/api/collections/links/records"
        , expect = Http.expectJson GotPaginatedResponse decodePaginatedResponse
        }
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        navLinks =
            viewNavLinks model.links
    in
    { title = "Stamp | Send Mail With Code"
    , content =
        div []
            [ viewHeader viewNavButton navLinks
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


viewNavLinks : Status -> Html msg
viewNavLinks status =
    case status of
        Failure ->
            div []
                [ text "Failed to load links" ]

        Loading ->
            div []
                [ text "Fetching links" ]

        Success paginatedResponse ->
            ul []
                (List.map (\link -> li [] [ viewLink link.url link.title ]) (paginatedResponseToLinks paginatedResponse))


viewNavButton : Html Msg
viewNavButton =
    viewButtonImage
        [ class "block w-6 h-6"
        ]
        NavToggle
        "/static/img/menu.svg"



-- UPDATE


type Msg
    = GotPaginatedResponse (Result Http.Error PaginatedResponse)
    | NavToggle


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

        NavToggle ->
            ( { model | navOpen = not model.navOpen }
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
        |> custom (at [ "items" ] (list decodeCollectionResponse))


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
