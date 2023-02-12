port module Port exposing
    ( addGetSession
    , getSession
    , recieveSession
    , setSession
    )

-- PORTS


port setSession : ( String, Int ) -> Cmd msg


port getSession : () -> Cmd msg


port recieveSession : (( String, String, Int ) -> msg) -> Sub msg



-- HELPERS


addGetSession : ( model, Cmd msg ) -> ( model, Cmd msg )
addGetSession ( model, cmd ) =
    ( model, Cmd.batch [ cmd, getSession () ] )
