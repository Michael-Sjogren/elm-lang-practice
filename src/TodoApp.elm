module TodoApp exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model


type alias TodoItem =
    { description : String }


type alias Model =
    { todos : List TodoItem
    , tempTodo : TodoItem
    }


init : Model
init =
    { todos = []
    , tempTodo = { description = "" }
    }


type Msg
    = CreatedTodo TodoItem
    | EditTempTodo String
    | DeletedTodo TodoItem
    | Nothing



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditTempTodo desc ->
            { model | tempTodo = { description = desc } }

        CreatedTodo newTodoItem ->
            { model
                | todos = List.append model.todos [ newTodoItem ]
                , tempTodo = init.tempTodo
            }

        DeletedTodo todoItem ->
            { model | todos = List.filter (\a -> not(a == todoItem) ) model.todos }
            
        Nothing ->
            model



-- View


todoItemHtml : TodoItem -> Html Msg
todoItemHtml todo =
    div []
        [ 
            span [ onClick (DeletedTodo todo) ] [ text todo.description ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Todo Application in Elm!" ]
        , div []
            [ input [ placeholder "Description", type_ "text", value model.tempTodo.description, onInput EditTempTodo ] []
            , button [ onClick (CreatedTodo model.tempTodo) ] [ text "Add Todo" ]
            ]
        , ol []
            (List.map todoItemHtml model.todos)
        ]
