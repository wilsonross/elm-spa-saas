module Helper exposing (maybeAttribute)

import Html exposing (Attribute)


maybeAttribute : Maybe a -> List (Attribute msg) -> List (Attribute msg)
maybeAttribute maybe attr =
    case maybe of
        Just _ ->
            attr

        Nothing ->
            []
