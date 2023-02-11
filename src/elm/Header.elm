module Header exposing
    ( Model
    , Msg(..)
    , PaginatedResponse
    , Status(..)
    , init
    , paginatedResponseToLinks
    , sortLinks
    , update
    , view
    )

import Compare exposing (Comparator)
import Html exposing (Html, button, div, form, img, input, li, text, ul)
import Html.Attributes exposing (class, placeholder, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, at, int, list, string)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Request
import Session exposing (Session(..))
import View exposing (Link, viewButtonImage, viewLink, viewLinkImage, viewLogo)



-- MODEL


type alias Model =
    { links : Status
    , navOpen : Bool
    , mobileSearchOpen : Bool
    , path : String
    , auth : Bool
    }


type Status
    = Failure
    | Loading
    | Success PaginatedResponse


init : String -> String -> Bool -> ( Model, Cmd Msg )
init apiUrl currentPath userAuth =
    ( { links = Loading
      , navOpen = False
      , mobileSearchOpen = False
      , path = currentPath
      , auth = userAuth
      }
    , Http.get
        { url = Request.joinUrl apiUrl "/api/collections/links/records"
        , expect = Http.expectJson GotPaginatedResponse decodePaginatedResponse
        }
    )



-- UPDATE


type Msg
    = GotPaginatedResponse (Result Http.Error PaginatedResponse)
    | NavToggle
    | MobileSearchToggle


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

        MobileSearchToggle ->
            ( { model | mobileSearchOpen = not model.mobileSearchOpen }
            , Cmd.none
            )



-- HELPERS


paginatedResponseToLinks : PaginatedResponse -> List Link
paginatedResponseToLinks response =
    List.map
        (\record ->
            { url = record.href
            , title = record.title
            , sortOrder = record.order
            }
        )
        response.items



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class <|
            "flex h-[var(--header-height)] mx-auto px-12 lg:px-5 items-center"
                ++ " w-full gap-6 sm:gap-9 max-w-[76.5rem]"
        ]
        [ viewLogo []
        , viewDesktopSearch
        , viewMobileSearch model
        , viewAuthLinks model.auth
        , viewNavButton
        , viewOverlay model
        , viewNav model
        ]


viewMobileSearch : Model -> Html Msg
viewMobileSearch model =
    let
        searchClass =
            if not model.mobileSearchOpen then
                " -translate-y-[200%]"

            else
                ""
    in
    div [ class "md:hidden" ]
        [ viewButtonImage [ class "h-5 w-5" ]
            MobileSearchToggle
            "/static/img/search.svg"
        , form
            [ class <|
                "bg-white rounded-md px-[13px] h-12 items-center flex"
                    ++ " top-3 right-6 left-6 fixed transition-transform"
                    ++ " duration-300 shadow-modal"
                    ++ searchClass
            ]
            [ input
                [ placeholder "Find design"
                , class <|
                    "focus-visible:outline-none w-full pt-[2px]"
                        ++ " placeholder:grey-2"
                ]
                []
            , viewButtonImage [ class "h-5 w-5" ]
                MobileSearchToggle
                "/static/img/close.svg"
            ]
        ]


viewDesktopSearch : Html Msg
viewDesktopSearch =
    form
        [ class <|
            "bg-grey-0 rounded-md px-[11px] w-[16.625rem] h-10 hidden md:flex"
                ++ " items-center"
        ]
        [ input
            [ placeholder "Find design"
            , class <|
                "bg-grey-0 focus-visible:outline-none w-full pt-[2px]"
                    ++ " placeholder:grey-2"
            ]
            []
        , button [ class "h-5 w-5 shrink-0" ]
            [ img [ src "/static/img/search.svg" ] []
            ]
        ]


viewAuthLinks : Bool -> Html Msg
viewAuthLinks auth =
    if auth == True then
        viewAuthenticatedLinks

    else
        viewUnauthenticatedLinks


viewAuthenticatedLinks : Html msg
viewAuthenticatedLinks =
    viewLinkImage [ class "w-[62px] h-[18px]" ]
        "/account"
        "/static/img/account.svg"


viewUnauthenticatedLinks : Html msg
viewUnauthenticatedLinks =
    div [ class "flex gap-6 sm:gap-9" ]
        [ viewLinkImage [ class "w-[41px] h-[18px]" ]
            "/login"
            "/static/img/login.svg"
        , viewLinkImage [ class "w-[61px] h-[18px] hidden sm:block" ]
            "/register"
            "/static/img/register.svg"
        ]


viewNavButton : Html Msg
viewNavButton =
    viewButtonImage
        [ class "block w-6 h-6"
        ]
        NavToggle
        "/static/img/menu.svg"


viewOverlay : Model -> Html Msg
viewOverlay model =
    let
        overlayClass =
            if not model.navOpen then
                " opacity-0 pointer-events-none"

            else
                " opacity-[.14]"
    in
    div
        [ class <|
            "fixed inset-0 bg-black transition-opacity duration-500 z-20"
                ++ overlayClass
        , onClick NavToggle
        ]
        []


viewNav : Model -> Html Msg
viewNav model =
    let
        navClass =
            if not model.navOpen then
                " translate-x-full"

            else
                ""
    in
    div
        [ class <|
            "fixed top-0 right-0 bottom-0 max-w-[21.25rem] duration-500"
                ++ " transition-transform bg-white px-[3.125rem] py-7 w-full"
                ++ " bg-navblur bg-no-repeat bg-cover bg-right-bottom z-30"
                ++ navClass
        ]
        [ viewNavCloseButton
        , viewNavLinks model
        ]


viewNavCloseButton : Html Msg
viewNavCloseButton =
    viewButtonImage
        [ class "block w-6 h-6 ml-auto"
        ]
        NavToggle
        "/static/img/close.svg"


viewNavLinks : Model -> Html Msg
viewNavLinks model =
    let
        status =
            model.links
    in
    case status of
        Failure ->
            div [ class "h-full flex items-center justify-center text-grey-2" ]
                [ text "Failed to load links" ]

        Loading ->
            div [ class "h-full flex items-center justify-center text-grey-2" ]
                [ text "Fetching links" ]

        Success paginatedResponse ->
            let
                sortedLinks =
                    sortLinks (paginatedResponseToLinks paginatedResponse)
            in
            ul [ class "mt-32" ]
                (List.map
                    (\link ->
                        viewNavLink link model
                    )
                    sortedLinks
                )


viewNavLink : Link -> Model -> Html Msg
viewNavLink link model =
    li [ class "mb-8 flex items-center justify-end gap-[.875rem]" ]
        [ viewActiveNavLink model.path link
        , div []
            [ viewLink
                [ class "text-xl align-text-top"
                ]
                link.url
                link.title
            ]
        ]


viewActiveNavLink : String -> Link -> Html Msg
viewActiveNavLink path link =
    let
        circle =
            if path == link.url then
                img
                    [ src "/static/img/circle.svg"
                    , class "w-full h-full block shrink-0"
                    ]
                    []

            else
                text ""
    in
    div [ class "w-2 h-2" ]
        [ circle
        ]



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



-- SORTING


nameComparator : Comparator Link
nameComparator =
    Compare.by .title


sortOrderComparator : Comparator Link
sortOrderComparator =
    Compare.by .sortOrder


linkComparator : Comparator Link
linkComparator =
    Compare.concat [ sortOrderComparator, nameComparator ]


sortLinks : List Link -> List Link
sortLinks =
    List.sortWith linkComparator
