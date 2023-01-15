module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, li, p, text, ul)
import Session exposing (Session)
import View exposing (viewFooter, viewLink)



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Send Mail With Code"
    , content =
        div []
            [ div []
                [ ul []
                    [ li [] [ viewLink [] "/" "Home" ]
                    , li [] [ viewLink [] "/login" "Login" ]
                    , li [] [ viewLink [] "/register" "Register" ]
                    ]
                , p [] [ text "Home" ]
                ]
            , viewFooter
            ]
    }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )
