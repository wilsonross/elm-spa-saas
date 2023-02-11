module Route exposing (Route(..), fromUrl, protected)

import Browser.Navigation as Nav
import Session exposing (Session(..))
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
    | ForgotPassword


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
        , Parser.map ForgotPassword (s "forgot-password")
        ]



-- HELPERS


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = url.path, fragment = Nothing }
        |> Parser.parse parser


protected : Session -> Bool -> Cmd msg
protected session invert =
    case session of
        User _ ->
            if invert then
                Nav.pushUrl (Session.navKey session) "/account"

            else
                Cmd.none

        _ ->
            if invert then
                Cmd.none

            else
                Nav.pushUrl (Session.navKey session) "/login"
