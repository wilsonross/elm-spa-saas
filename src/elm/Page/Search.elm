module Page.Search exposing (Model, Msg, init, update, view)

import Header exposing (Status)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Page exposing (viewComponent)
import Request exposing (Status(..))
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , header : Header.Model
    , response : Status ()
    , query : String
    }


init : Session -> String -> ( Model, Cmd Msg )
init session query =
    let
        ( header, subMsg ) =
            Header.init session
    in
    ( { session = session
      , header = header
      , response = Loading
      , query = query
      }
    , Cmd.batch
        [ Cmd.map GotHeaderMsg subMsg ]
    )



-- UPDATE


type Msg
    = GotHeaderMsg Header.Msg
    | GotResponse (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHeaderMsg subMsg ->
            Header.update subMsg model.header
                |> updateWith model GotHeaderMsg

        GotResponse result ->
            case result of
                Ok _ ->
                    ( { model | response = Response () }, Cmd.none )

                Err _ ->
                    ( { model | response = Failure }
                    , Cmd.none
                    )


updateWith : Model -> (subMsg -> Msg) -> ( Header.Model, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith model toMsg ( subModel, subCmd ) =
    ( { model | header = subModel }
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Stamp | Results"
    , content =
        div [ class "h-full" ]
            [ viewHeader model
            , div
                [ class <|
                    "px-6 sm:px-12 lg:px-5 w-full mx-auto max-w-[76.5rem] pt-5"
                        ++ " lg:pt-0 mt-6 md:mt-28 mb-20"
                ]
                []
            ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    Header.view model.header
        |> viewComponent GotHeaderMsg
