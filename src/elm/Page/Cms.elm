module Page.Cms exposing (Model, Msg, init, update, view)

import Header exposing (Status(..))
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import Page exposing (viewComponent)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    , identifier : String
    , title : String
    , content : String
    }


init : Session -> String -> ( Model, Cmd Msg )
init session identifier =
    let
        ( header, subMsg ) =
            Header.init session
    in
    ( { session = session
      , header = header
      , title = "Cms"
      , content = "Content"
      , identifier = identifier
      }
    , Cmd.map GotHeaderMsg subMsg
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
                ++ " pb-[4.25rem] px-20 shadow-page"
        ]
        [ viewTitle model.title
        , viewContent model.content
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
    p
        [ class "text-sm leading-[1.625rem]" ]
        [ text content ]



-- UPDATE


type Msg
    = GotHeaderMsg Header.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHeaderMsg subMsg ->
            Header.update subMsg model.header
                |> updateWith model GotHeaderMsg


updateWith : Model -> (subMsg -> Msg) -> ( Header.Model, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith model toMsg ( subModel, subCmd ) =
    ( { model | header = subModel }
    , Cmd.map toMsg subCmd
    )
