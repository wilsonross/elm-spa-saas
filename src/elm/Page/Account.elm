module Page.Account exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, div, form, text)
import Html.Attributes exposing (class)
import Html.Parser as Parser
import Html.Parser.Util as HtmlParserUtil
import Http
import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (optional, required)
import Port
import Request exposing (Status(..), viewPreloader)
import Response
    exposing
        ( ErrorDetailed(..)
        , JsonResponse(..)
        )
import Route
import Session exposing (Session)
import View
    exposing
        ( viewAlternative
        , viewAuthLogo
        , viewButtonImage
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , messageResponse : Status MessageResponse
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , messageResponse = Loading
      }
    , Cmd.batch
        [ Route.protected session False
        , Http.get
            { url =
                Request.joinUrl
                    (Session.apiUrl session)
                    "/api/collections/cms/records/"
                    ++ Session.accountMessageId session
            , expect =
                Http.expectJson
                    GotMessageResponse
                    decodeResponse
            }
        ]
    )



-- UPDATE


type Msg
    = GotMessageResponse (Result Http.Error MessageResponse)
    | Logout
    | Delete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMessageResponse result ->
            case result of
                Ok res ->
                    updateModelOnSuccessResponse model res

                Err _ ->
                    ( { model | messageResponse = Failure }, Cmd.none )

        Logout ->
            handleLogout model

        Delete ->
            ( model, Cmd.none )


updateModelOnSuccessResponse : Model -> MessageResponse -> ( Model, Cmd msg )
updateModelOnSuccessResponse model response =
    ( { model
        | messageResponse = Request.Response response
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Login"
    , content =
        div
            [ class <|
                "flex justify-center items-center h-screen rounded-md px-4"
            ]
            [ viewAccount model
            ]
    }


viewAccount : Model -> Html Msg
viewAccount model =
    form
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[3.125rem]"
                ++ " pb-[4.059rem] sm:px-10 relative rounded-md relative"
                ++ " overflow-hidden"
        ]
        [ viewAuthLogo
        , viewTitle "Account"
        , viewContentOrError model.messageResponse
        , viewButtonGroup
        , viewAlternative "Return to " "homepage" "" "/"
        , viewPreloader model.messageResponse
        ]


viewContentOrError : Status MessageResponse -> Html msg
viewContentOrError status =
    case status of
        Request.Response response ->
            viewContent response.content

        _ ->
            text ""


viewContent : String -> Html msg
viewContent content =
    let
        contentHtml =
            Parser.run content
    in
    case contentHtml of
        Ok html ->
            div
                [ class "text-xs leading-[1.125rem] mt-6 mb-9 links-turq"
                ]
                (HtmlParserUtil.toVirtualDom html)

        Err _ ->
            text content


viewButtonGroup : Html Msg
viewButtonGroup =
    div
        [ class "flex gap-6 mb-4" ]
        [ viewButtonImage [] Logout "/static/img/logout.svg"
        , viewButtonImage [] Delete "/static/img/delete.svg"
        ]



-- HELPERS


handleLogout : Model -> ( Model, Cmd Msg )
handleLogout model =
    ( { model | session = Session.forceGuestSession model.session }
    , Cmd.batch
        [ Port.setSession ( "", 0 )
        , Nav.pushUrl (Session.navKey model.session) "/"
        ]
    )



-- JSON


type alias MessageResponse =
    { collectionId : String
    , collectionName : String
    , content : String
    , created : String
    , id : String
    , image : String
    , searchable : Bool
    , tagline : String
    , title : String
    , updated : String
    }


decodeResponse : Decoder MessageResponse
decodeResponse =
    Decode.succeed MessageResponse
        |> required "collectionId" string
        |> required "collectionName" string
        |> required "content" string
        |> required "created" string
        |> required "id" string
        |> optional "image" string ""
        |> required "searchable" bool
        |> required "tagline" string
        |> required "title" string
        |> required "updated" string
