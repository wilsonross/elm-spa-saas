module Page.Home exposing (Model, Msg, init, update, view)

import Header exposing (CollectionResponse, Status(..))
import Html exposing (Html, div, h1, img, p, span, text)
import Html.Attributes exposing (class, src)
import Page exposing (viewComponent)
import Response exposing (PaginatedResponse)
import Session exposing (Session)
import View exposing (Link, viewLink)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( header, subMsg ) =
            Header.init session
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
                [ div
                    [ class "h-full lg:w-1/2"
                    ]
                    [ viewCta (Session.toFlag model.session (\f -> f.cmsCtaId))
                    , viewCarouselLoader model.header.links
                    ]
                , viewGraphic
                ]
            ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    Header.view model.header
        |> viewComponent GotHeaderMsg


viewCta : String -> Html msg
viewCta linkId =
    div
        [ class <|
            "flex flex-col justify-center mb-7 sm:mb-[3.75rem] md:mb-16"
                ++ " lg:mb-0 h-[calc(100%_-_var(--home-carousel-height))]"
        ]
        [ viewTitle, viewDescription, viewButton linkId ]


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


viewButton : String -> Html msg
viewButton linkId =
    viewLink
        [ class <|
            "h-[3.375rem] rounded-md bg-black flex sm:w-fit text-white"
                ++ " justify-center items-center px-[1.125rem] font-semibold"
                ++ " text-lg leading-[1.625rem]"
        ]
        ("/cms/" ++ linkId)
        "Lets Talk Business"


viewCarouselLoader : Status -> Html msg
viewCarouselLoader status =
    case status of
        Failure ->
            text ""

        Loading ->
            text ""

        Success paginatedResponse ->
            viewCarousel (formatLinks paginatedResponse)


viewCarousel : List Link -> Html msg
viewCarousel links =
    div
        [ class <|
            "hidden lg:flex mb-[3.75rem] lg:mb-0 overflow-hidden"
                ++ " psuedo-fade-r psuedo-fade-l items-center mr-10"
        ]
        [ div
            [ class <|
                "flex items-center animate-carousel hover:pause no-wrap"
                    ++ " will-change-transform"
            ]
            (List.map (\link -> viewCarouselLink link) links)
        ]


viewCarouselLink : Link -> Html msg
viewCarouselLink link =
    div [ class "px-[0.375rem]" ]
        [ viewLink
            [ class <|
                "bg-grey-1 rounded-full px-[0.875rem] capitalize hover:bg-turq"
                    ++ " leading-[2.625rem] hover:text-white transition-colors"
                    ++ " duration-200 ease-in block whitespace-nowrap"
            ]
            link.url
            link.title
        ]


viewGraphic : Html msg
viewGraphic =
    img
        [ src "/static/img/graphic.svg"
        , class <|
            "lg:w-1/2 lg:mb-[3.75rem] lg:mb-16 max-w-xl lg:max-w-xl"
                ++ " md:self-end lg:self-center"
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



-- HELPERS


doubleLinks : List Link -> List Link
doubleLinks links =
    links ++ links


removeIndexLink : List Link -> List Link
removeIndexLink links =
    List.filter (\link -> link.url /= "/") links


formatLinks : PaginatedResponse (List CollectionResponse) -> List Link
formatLinks paginatedResponse =
    Header.paginatedResponseToLinks paginatedResponse
        |> removeIndexLink
        |> Header.sortLinks
        |> doubleLinks
