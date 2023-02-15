module Page.Account exposing (Model, Msg, init, update, view)

import Auth
import Browser.Navigation as Nav
import Html exposing (Html, button, div, form, img, text)
import Html.Attributes exposing (class, src)
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
        , viewErrors
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    , messageResponse : Status MessageResponse
    , deleteResponse : Status ()
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , messageResponse = Loading
      , deleteResponse = None
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
                    decodeMessageResponse
            }
        ]
    )



-- UPDATE


type Msg
    = GotMessageResponse (Result Http.Error MessageResponse)
    | GotDeleteResponse (Result Http.Error ())
    | Logout
    | Delete
    | ResetDeleteError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMessageResponse result ->
            case result of
                Ok res ->
                    updateModelOnSuccessResponse model res

                Err _ ->
                    ( { model | messageResponse = Failure }, Cmd.none )

        GotDeleteResponse result ->
            case result of
                Ok _ ->
                    handleLogout model

                Err _ ->
                    ( { model | deleteResponse = Failure }
                    , View.delay 2500 ResetDeleteError
                    )

        Logout ->
            handleLogout model

        Delete ->
            ( model
            , Auth.delete GotDeleteResponse model.session
            )

        ResetDeleteError ->
            ( { model | deleteResponse = None }, Cmd.none )


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
            , viewCouldNotDelete model.deleteResponse
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
        , viewPreloader
            (Request.isLoading
                model.messageResponse
                model.deleteResponse
            )
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
            viewMessageError


viewMessageError : Html msg
viewMessageError =
    div [ class "justify-center text-grey-2 text-center" ]
        [ text "Could not load message" ]


viewButtonGroup : Html Msg
viewButtonGroup =
    div
        [ class "sm:flex sm:gap-6 mb-4" ]
        [ viewButton Logout "/static/img/logout.svg"
        , viewButton Delete "/static/img/delete.svg"
        ]


viewButton : msg -> String -> Html msg
viewButton msg image =
    div
        [ class <|
            "bg-black rounded  h-[3.25rem] mb-5 sm:mb-0 last:mb-0 sm:w-1/2"
        ]
        [ button
            [ View.preventDefault msg
            , class <|
                "block shrink-0 focus:outline-none flex justify-center w-full"
                    ++ " items-center h-full"
            ]
            [ img
                [ src image
                , class "block"
                ]
                []
            ]
        ]


viewCouldNotDelete : Status () -> Html msg
viewCouldNotDelete status =
    case status of
        Failure ->
            viewErrors
                [ { code = ""
                  , message = "Could not delete"
                  }
                ]

        _ ->
            text ""



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


decodeMessageResponse : Decoder MessageResponse
decodeMessageResponse =
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
