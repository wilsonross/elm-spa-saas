module Page.Home exposing (Model, Msg, init, update, view)

import Header
import Html exposing (Html, div, h1, img, p, span, text)
import Html.Attributes exposing (class, src)
import Page exposing (viewComponent)
import Session exposing (Session, apiUrl, pathFromSession)
import View exposing (viewLink)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( header, subMsg ) =
            Header.init (apiUrl session) (pathFromSession session)
    in
    ( { session = session
      , header = header
      }
    , Cmd.map GotHeaderMsg subMsg
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Send Mail With Code"
    , content =
        div [ class "h-full" ]
            [ viewHeader model
            , div
                [ class <|
                    "flex px-12 lg:px-5 max-w-[76.5rem] w-full lg:items-center"
                        ++ " lg:h-[calc(100vh_-_var(--header-height))] mx-auto"
                        ++ " flex-col lg:flex-row pt-5 lg:pt-0"
                ]
                [ viewCta
                , viewGraphic
                ]
            ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    viewComponent GotHeaderMsg (Header.view model.header)


viewCta : Html msg
viewCta =
    div [ class "lg:w-1/2 mb-[3.75rem] lg:mb-0" ]
        [ viewTitle, viewDescription, viewButton ]


viewTitle : Html msg
viewTitle =
    h1
        [ class <|
            "flex flex-col text-5xl sm:text-[4.125rem] md:text-[5.5rem]"
                ++ " leading-[3rem] sm:leading-[4rem] md:leading-[5.375rem]"
                ++ " font-black mb-4"
        ]
        [ span [ class "uppercase" ] [ text "you’ve" ]
        , span [ class "uppercase text-turq" ] [ text "got" ]
        , span [ class "uppercase" ] [ text "mail." ]
        ]


viewDescription : Html msg
viewDescription =
    p
        [ class <|
            "text-base sm:text-xl font-medium mb-5 sm:mb-10"
                ++ " lg:max-w-[25rem] "
        ]
        [ text <|
            "Select a design or upload your own, send mail at the click of a"
                ++ " button or code 💅"
        ]


viewButton : Html msg
viewButton =
    viewLink
        [ class <|
            "h-[3.375rem] rounded-md bg-black flex sm:inline-flex text-white"
                ++ " justify-center items-center px-[1.125rem] font-semibold"
                ++ " text-lg leading-[1.625rem]"
        ]
        "/about"
        "Lets Talk Buisness"


viewGraphic : Html msg
viewGraphic =
    img
        [ src "/static/img/graphic.svg"
        , class "lg:w-1/2 mb-[3.75rem] lg:mb-0"
        ]
        []



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
