module Page.Home exposing (Model, Msg, init, view)

import Html exposing (Html, div, li, p, text, ul)
import Session exposing (Session)
import View exposing (viewLink)



-- MODEL


type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html msg }
view _ =
    { title = "Stamp | Send Mail With Code"
    , content =
        div []
            [ ul []
                [ li [] [ viewLink "/" "Home" ]
                , li [] [ viewLink "/login" "Login" ]
                , li [] [ viewLink "/register" "Register" ]
                ]
            , p [] [ text "Home" ]
            ]
    }



-- UPDATE


type Msg
    = Msg
