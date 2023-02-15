module Page.Account exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, form)
import Html.Attributes exposing (class)
import Request exposing (Status(..))
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
        , viewTitle
        )



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Route.protected session False
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



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
viewAccount _ =
    form
        [ class <|
            "bg-white max-w-md w-full shadow-portal px-[1.25rem] pt-[3.125rem]"
                ++ " pb-[4.059rem] sm:px-10 relative rounded-md"
                ++ " overflow-hidden"
        ]
        [ viewAuthLogo
        , viewTitle "Account"
        , viewAlternative "Return to " "homepage" "" "/"
        ]
