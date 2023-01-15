module Page.Home exposing (Model, Msg, init, update, view)

import Header
import Html exposing (Html, div, li, p, text, ul)
import Page exposing (viewComponent)
import Session exposing (Session, apiUrl, pathFromSession)
import View exposing (viewFooter, viewLink)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( header, _ ) =
            Header.init (apiUrl session) (pathFromSession session)
    in
    ( { session = session
      , header = header
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Send Mail With Code"
    , content =
        div []
            [ viewHeader model
            , div []
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


viewHeader : Model -> Html Msg
viewHeader model =
    viewComponent GotHeaderMsg (Header.view model.header)



-- UPDATE


type Msg
    = GotHeaderMsg Header.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHeaderMsg subMsg ->
            Header.update subMsg model.header
                |> updateWith model GotHeaderMsg


updateWith : Model -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith model toMsg ( subModel, subCmd ) =
    ( model
    , Cmd.map toMsg subCmd
    )
