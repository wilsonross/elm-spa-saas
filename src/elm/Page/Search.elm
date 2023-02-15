module Page.Search exposing (Model, Msg, init, update, view)

import Header exposing (Status)
import Html exposing (Html, a, div, h1, h3, img, p, text)
import Html.Attributes exposing (class, href, src)
import Http
import Page exposing (viewComponent)
import Request exposing (Status(..))
import Response exposing (CmsResponse, PaginatedResponse)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    , response : Status SearchResponse
    , query : String
    }


type alias SearchResponse =
    PaginatedResponse (List CmsResponse)


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
                        Response.decodeCmsResponse
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
                    "px-12 lg:px-0 w-full mx-auto max-w-[76.5rem] mb-20 mt-6"
                        ++ " md:mt-28"
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
        , viewSearchItems model.response
        ]


viewTitle : String -> Html msg
viewTitle query =
    h1
        [ class "text-2xl text-center font-medium mb-9" ]
        [ capitalize query
            |> addQuotes
            |> text
        ]


viewSearchItems : Status SearchResponse -> Html msg
viewSearchItems status =
    case status of
        Response response ->
            if List.isEmpty response.items then
                viewNoResults

            else
                div [ class "max-w-md w-full mx-auto" ]
                    (List.map viewSearchItem response.items)

        Failure ->
            viewNoResults

        _ ->
            div
                []
                []


viewSearchItem : CmsResponse -> Html msg
viewSearchItem collectionResponse =
    a
        [ href ("/cms/" ++ collectionResponse.id)
        , class <|
            "p-[1.125rem] block w-full flex rounded-md bg-white rounded-md"
                ++ " mb-[1.125rem] shadow-page items-center last:mb-0"
        ]
        [ viewSearchItemImage collectionResponse.image
        , viewSearchItemBody collectionResponse
        ]


viewSearchItemImage : String -> Html msg
viewSearchItemImage url =
    if url == "" then
        div
            [ class "w-16 h-16 shrink-0 bg-grey-1 hidden sm:block" ]
            []

    else
        img
            [ src url, class "w-16 h-16 shrink-0 hidden sm:block" ]
            []


viewSearchItemBody : CmsResponse -> Html msg
viewSearchItemBody collectionResponse =
    div
        [ class "p-0 sm:pl-[1.125rem] flex flex-col justify-center gap-[6px]" ]
        [ viewSearchItemTitle collectionResponse.title
        , viewSearchItemTagline collectionResponse.tagline
        ]


viewSearchItemTitle : String -> Html msg
viewSearchItemTitle title =
    h3
        [ class <|
            "text-xl font-medium relative after:content-[''] after:block"
                ++ " after:absolute after:w-[0.875rem] after:h-[2px]"
                ++ " after:bg-turq after:rounded-full after:left-px"
                ++ " after:bottom-px"
        ]
        [ text title ]


viewSearchItemTagline : String -> Html msg
viewSearchItemTagline tagline =
    p
        [ class "text-sm leading-[1.625rem]" ]
        [ text tagline ]


viewNoResults : Html msg
viewNoResults =
    div [ class "justify-center text-grey-2 text-center" ]
        [ text "No results found" ]



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
    "/api/collections/cms/records?filter=(title~'"
        ++ query
        ++ "'||tagline~'"
        ++ query
        ++ "')"
