module Main exposing(..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Maybe exposing (andThen)
import Bitwise exposing (or)
import Char exposing (isDigit)
import Char exposing (isUpper)
import Char exposing (isLower)



-- MAIN


main : Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewPasswordValidation model
    , viewNameValidation model
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewNameValidation : Model -> Html msg
viewNameValidation model =
    if String.length model.name < 2 then
      div [ style "color" "red" ] [ text "Invalid Name" ]
    else
      div [ style "color" "green" ] [ text "Name ok!" ]

validatePassword : Model -> (Bool , String)
validatePassword model =
-- TODO FIX THIS lul
  if model.passwordAgain == model.password && 
    String.length model.password > 8 &&
    (String.any isDigit model.password &&
    String.any isLower model.password &&
    String.any isUpper model.password)  then
    (True , "OK!")
  else
    (False , "Invalid Password")

viewPasswordValidation : Model -> Html msg
viewPasswordValidation model =
    let result = validatePassword model
    in
      if not (Tuple.first result) then
        div [ style "color" "red" ] [ text (Tuple.second result) ]
      else
        div [ style "color" "green" ] [ text (Tuple.second result) ]