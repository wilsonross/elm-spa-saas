port module Cookie exposing (Cookie, Msg, update)

-- MODEL


type alias Cookie =
    ( String, String )



-- PORTS


port setCookie : ( String, String, Int ) -> Cmd msg


port getCookie : String -> Cmd msg


port recieveCookie : (( String, String ) -> msg) -> Sub msg



-- UPDATE


type Msg
    = SetCookie Cookie Int
    | GetCookie String
    | RecieveCookie Cookie


update : Msg -> ( Maybe Cookie, Cmd Msg )
update msg =
    case msg of
        SetCookie ( key, value ) daysUntilExpiry ->
            ( Nothing, setCookie ( key, value, daysUntilExpiry ) )

        GetCookie key ->
            ( Nothing, getCookie key )

        RecieveCookie cookie ->
            ( Just cookie, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    recieveCookie RecieveCookie
