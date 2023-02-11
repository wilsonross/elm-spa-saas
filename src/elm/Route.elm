module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)



-- ROUTES


type Route
    = Home
    | Login
    | Register
    | Account
    | About
    | HowItWorks
    | Testimonials
    | Contact
    | Pricing


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Register (s "register")
        , Parser.map Account (s "account")
        , Parser.map About (s "about")
        , Parser.map HowItWorks (s "how-it-works")
        , Parser.map Testimonials (s "testimonials")
        , Parser.map Contact (s "contact")
        , Parser.map Pricing (s "pricing")
        ]



-- HELPERS


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = url.path, fragment = Nothing }
        |> Parser.parse parser
