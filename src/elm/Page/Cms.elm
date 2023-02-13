module Page.Cms exposing (Model, Msg, init, update, view)

import Header exposing (Status)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Html.Parser as Parser
import Html.Parser.Util as HtmlParserUtil
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Page exposing (viewComponent)
import Request exposing (Status(..), viewPreloader)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    , response : Status Response
    , id : String
    , title : String
    , content : String
    }


init : Session -> String -> ( Model, Cmd Msg )
init session id =
    let
        ( header, subMsg ) =
            Header.init session
    in
    ( { session = session
      , header = header
      , response = Loading
      , title = "Cms"
      , content = "Content"
      , id = id
      }
    , Cmd.batch
        [ Cmd.map GotHeaderMsg subMsg
        , Http.get
            { url =
                Request.joinUrl
                    (Session.apiUrl session)
                    ("/api/collections/cms/records/"
                        ++ id
                    )
            , expect =
                Http.expectJson
                    GotResponse
                    decodeResponse
            }
        ]
    )



-- UPDATE


type Msg
    = GotHeaderMsg Header.Msg
    | GotResponse (Result Http.Error Response)


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


updateModelOnSuccessResponse : Model -> Response -> ( Model, Cmd msg )
updateModelOnSuccessResponse model response =
    ( { model
        | response = Request.Response response
        , title = response.title
        , content = response.content
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
    { title = "Stamp | " ++ model.title
    , content =
        div [ class "h-full" ]
            [ viewHeader model
            , div
                [ class <|
                    "flex px-12 lg:px-5 max-w-[76.5rem] w-full lg:items-center"
                        ++ " lg:h-[calc(100vh_-_var(--header-height))] mx-auto"
                        ++ " flex-col lg:flex-row pt-5 lg:pt-0"
                ]
                [ viewCms model
                ]
            ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    Header.view model.header
        |> viewComponent GotHeaderMsg


viewCms : Model -> Html msg
viewCms model =
    div
        [ class <|
            "max-w-[39.5rem] w-full mx-auto bg-white rounded-md pt-[3.75rem]"
                ++ " pb-[4.25rem] px-20 shadow-page relative"
        ]
        [ viewTitle model.title
        , viewContent model.content
        , viewPreloader model.response
        ]


viewTitle : String -> Html msg
viewTitle title =
    h1
        [ class <|
            "text-xl mb-9 font-medium relative after:content-[''] after:block"
                ++ " after:absolute after:w-[0.875rem] after:h-[2px]"
                ++ " after:bg-turq after:rounded-full after:left-px"
                ++ " after:bottom-px w-fit mx-auto"
        ]
        [ text title ]


viewContent : String -> Html msg
viewContent content =
    let
        contentHtml =
            Parser.run content
    in
    case contentHtml of
        Ok html ->
            div
                [ class "text-sm leading-[1.625rem]"
                ]
                (HtmlParserUtil.toVirtualDom
                    html
                )

        Err _ ->
            text content



-- JSON


type alias Response =
    { collectionId : String
    , collectionName : String
    , content : String
    , created : String
    , id : String
    , title : String
    , updated : String
    }


decodeResponse : Decoder Response
decodeResponse =
    Decode.succeed Response
        |> required "collectionId" string
        |> required "collectionName" string
        |> required "content" string
        |> required "created" string
        |> required "id" string
        |> required "title" string
        |> required "content" string
