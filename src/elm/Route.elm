module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)



-- ROUTES


type Route
    = Home
    | Login
    | Register
    | Error


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Register (s "register")
        , Parser.map Error (s "error")
        ]



-- HELPERS


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = url.path, fragment = Nothing }
        |> Parser.parse parser
