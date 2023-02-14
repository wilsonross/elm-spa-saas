module Route exposing (Route(..), fromUrl, protected)

import Browser.Navigation as Nav
import Session exposing (Session(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s, string)
import Url.Parser.Query as Query



-- ROUTES


type Route
    = Home
    | Login
    | Register
    | Account
    | Contact
    | Pricing
    | ForgotPassword
    | Cms String
    | Search (Maybe String)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Register (s "register")
        , Parser.map Account (s "account")
        , Parser.map Contact (s "contact")
        , Parser.map Pricing (s "pricing")
        , Parser.map ForgotPassword (s "forgot-password")
        , Parser.map Cms (s "cms" </> string)
        , Parser.map Search (s "search" <?> Query.string "q")
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
