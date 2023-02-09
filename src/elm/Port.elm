port module Port exposing
    ( Cookie
    , Msg
    , addGetSession
    , getSession
    , recieveSession
    , setSession
    , subscriptions
    , update
    )

-- MODEL


type alias Cookie =
    ( String, String )



-- PORTS


port setSession : ( String, Int ) -> Cmd msg


port getSession : () -> Cmd msg


port recieveSession : (( String, String ) -> msg) -> Sub msg



-- UPDATE


type Msg
    = RecieveSession Cookie


update : Msg -> ( Cookie, Cmd Msg )
update msg =
    case msg of
        RecieveSession cookie ->
            ( cookie, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    recieveSession RecieveSession



-- HELPERS


addGetSession : ( model, Cmd msg ) -> ( model, Cmd msg )
addGetSession ( model, cmd ) =
    ( model, Cmd.batch [ cmd, getSession () ] )
