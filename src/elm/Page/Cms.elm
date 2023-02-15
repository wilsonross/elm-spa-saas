module Page.Cms exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Header exposing (Status)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Html.Parser as Parser
import Html.Parser.Util as HtmlParserUtil
import Http
import Page exposing (viewComponent)
import Request exposing (Status(..), viewPreloader)
import Response exposing (CmsResponse)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    , response : Status CmsResponse
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
      , title = ""
      , content = ""
      , id = id
      }
    , Cmd.batch
        [ Cmd.map GotHeaderMsg subMsg
        , Api.cms Response.decodeCmsResponse GotCmsResponse session id
        ]
    )



-- UPDATE


type Msg
    = GotHeaderMsg Header.Msg
    | GotCmsResponse (Result Http.Error CmsResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHeaderMsg subMsg ->
            Header.update subMsg model.header
                |> updateWith model GotHeaderMsg

        GotCmsResponse result ->
            case result of
                Ok res ->
                    updateModelOnSuccessCmsResponse model res

                Err _ ->
                    ( { model | response = Failure }
                    , Nav.pushUrl (Session.navKey model.session) "/404"
                    )


updateModelOnSuccessCmsResponse : Model -> CmsResponse -> ( Model, Cmd msg )
updateModelOnSuccessCmsResponse model response =
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
                    "px-6 sm:px-12 lg:px-5 w-full mx-auto max-w-[76.5rem] mt-6"
                        ++ " md:mt-28 mb-20"
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
                ++ " pb-[4.25rem] px-6 md:px-20 shadow-page relative"
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
                [ class "text-sm leading-[1.625rem] links-turq"
                ]
                (HtmlParserUtil.toVirtualDom
                    html
                )

        Err _ ->
            text content
