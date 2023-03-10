module Input exposing
    ( Input(..)
    , canSubmit
    , checkEmail
    , encodeInput
    , inputBorder
    , invalidate
    , stringFromInput
    , valueToInput
    , viewCheckbox
    , viewInput
    , viewStatefulInput
    )

import Html exposing (Attribute, Html, div, img, input, text)
import Html.Attributes exposing (class, name, src, type_)
import Html.Events exposing (onCheck, onInput)
import Json.Encode as Encode


type Input
    = Empty
    | Invalid String
    | Valid String



-- HELPERS


inputToBool : Input -> Bool
inputToBool input =
    case input of
        Valid _ ->
            True

        _ ->
            False


stringFromInput : Input -> String
stringFromInput input =
    case input of
        Empty ->
            ""

        Invalid str ->
            str

        Valid str ->
            str


valueToInput : String -> (String -> Bool) -> Input
valueToInput val check =
    case val of
        "" ->
            Empty

        _ ->
            if check val then
                Valid val

            else
                Invalid val


inputBorder : Input -> Attribute msg
inputBorder input =
    case input of
        Invalid _ ->
            class "border-red-500"

        _ ->
            class "border-grey-3"


canSubmit : List Input -> Bool
canSubmit inputs =
    List.all inputToBool inputs


encodeInput : List ( String, Input ) -> Encode.Value
encodeInput encodingData =
    Encode.object
        (List.map
            (\( key, value ) -> ( key, stringFromInput value |> Encode.string ))
            encodingData
        )


invalidate : Input -> Input
invalidate input =
    Invalid (stringFromInput input)


checkEmail : String -> Bool
checkEmail email =
    String.contains "@" email && String.contains "." email



-- VIEW


viewInput : List (Attribute msg) -> String -> (String -> msg) -> Html msg
viewInput attr placeholder msg =
    input
        (attr
            ++ [ onInput msg
               , Html.Attributes.placeholder placeholder
               , class <|
                    "focus-visible:outline-none block h-10 w-full border"
                        ++ " px-[0.93rem] rounded text-sm transition-colors"
                        ++ " placeholder:text-grey-2 duration-700 ease-out"
               ]
        )
        []


viewStatefulInput : Input -> (String -> msg) -> List (Attribute msg) -> String -> Html msg
viewStatefulInput input msg attr placeholder =
    viewInput (inputBorder input :: attr) placeholder msg


viewCheckbox : (Bool -> msg) -> List (Attribute msg) -> Bool -> String -> String -> Html msg
viewCheckbox msg attr checked name label =
    div (class "flex items-center gap-2" :: attr)
        [ div
            [ class "w-[1.125rem] h-[1.125rem] relative"
            ]
            [ viewCheckboxHidden msg checked
            , viewCheckboxImageUnchecked
            , viewCheckboxImageChecked
            ]
        , viewLabel name label
        ]


viewCheckboxHidden : (Bool -> msg) -> Bool -> Html msg
viewCheckboxHidden msg checked =
    input
        [ class "block w-full h-full opacity-0"
        , type_ "checkbox"
        , onCheck msg
        , Html.Attributes.checked checked
        ]
        []


viewCheckboxImageUnchecked : Html msg
viewCheckboxImageUnchecked =
    img
        [ src "/static/img/unchecked.svg"
        , class <|
            "block absolute inset-0 checkbox-img pointer-events-none"
        ]
        []


viewCheckboxImageChecked : Html msg
viewCheckboxImageChecked =
    img
        [ src "/static/img/checked.svg"
        , class <|
            "block absolute inset-0 opacity-0 checkbox-img pointer-events-none"
        ]
        []


viewLabel : String -> String -> Html msg
viewLabel name label =
    Html.label
        [ class "text-xs leading-[1.125rem] capitalize"
        , Html.Attributes.name name
        ]
        [ text label ]
