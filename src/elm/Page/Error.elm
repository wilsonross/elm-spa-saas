module Page.Error exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, h2, h3, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Session exposing (Session)
import View
    exposing
        ( viewDescription
        , viewLink
        , viewRule
        )



-- MODEL


type alias Model =
    { session : Session
    , code : Int
    , message : String
    }


init : Session -> Int -> String -> ( Model, Cmd Msg )
init session code message =
    ( { session = session
      , code = code
      , message = message
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Back


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Back ->
            ( model
            , Nav.back
                (Session.navKey model.session)
                1
            )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | " ++ String.fromInt model.code
    , content =
        div
            [ class <|
                "flex justify-center items-center h-screen rounded-md px-4"
            ]
            [ viewModal model
            ]
    }


viewModal : Model -> Html Msg
viewModal model =
    div
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[7.75rem]"
                ++ " pb-[4.059rem] sm:px-10"
        ]
        [ viewHeading model.code
        , viewRule
        , viewCta model.message
        ]


viewError : Html msg
viewError =
    h2
        [ class <|
            "uppercase font-bold text-[2.5rem] leading-10 mb-2 text-center"
        ]
        [ text "Error" ]


viewTitle : Int -> Html msg
viewTitle code =
    h1
        [ class <|
            "text-[5.5rem] font-black text-turq leading-[5.375rem] text-center"
        ]
        [ text (String.fromInt code) ]


viewHeading : Int -> Html msg
viewHeading code =
    div
        []
        [ viewError
        , viewTitle code
        ]


viewAlternative : Html Msg
viewAlternative =
    div [ class "text-xs leading-[1.125rem] text-center" ]
        [ span [] [ text "Please go " ]
        , button [ class "text-turq", onClick Back ] [ text "back" ]
        , span [] [ text " or return to our " ]
        , viewLink [ class "text-turq" ] "/" "homepage"
        ]


viewCta : String -> Html Msg
viewCta description =
    div
        []
        [ viewDescription description
        , viewAlternative
        ]
