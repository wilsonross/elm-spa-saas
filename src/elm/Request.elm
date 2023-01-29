module Request exposing (Status(..))

-- MODEL


type Status res
    = None
    | Failure
    | Loading
    | Response res
