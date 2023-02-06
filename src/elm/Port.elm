port module Port exposing
    ( Cookie
    , Msg
    , addGetCookie
    , getCookie
    , recieveCookie
    , setCookie
    , subscriptions
    , update
    )

-- MODEL


type alias Cookie =
    ( String, String )



-- PORTS


port setCookie : ( String, String, Int ) -> Cmd msg


port getCookie : String -> Cmd msg


port recieveCookie : (( String, String ) -> msg) -> Sub msg



-- UPDATE


type Msg
    = RecieveCookie Cookie


update : Msg -> ( Cookie, Cmd Msg )
update msg =
    case msg of
        RecieveCookie cookie ->
            ( cookie, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    recieveCookie RecieveCookie



-- HELPERS


addGetCookie : String -> ( model, Cmd msg ) -> ( model, Cmd msg )
addGetCookie key ( model, cmd ) =
    ( model, Cmd.batch [ cmd, getCookie key ] )
