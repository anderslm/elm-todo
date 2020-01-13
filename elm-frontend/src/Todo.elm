module Todo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

---- MODEL ----
type alias Model =
    { newItem : String,
      items : List String }

init : ( Model, Cmd Msg )
init =
    ( { newItem = "", items = [] }, Cmd.none )

---- UPDATE ----
type Msg
    = NewItem String
    | AddItem
    | RemoveItem String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let _ = Debug.log "msg" msg in
    let newModel =
         case msg of
            NewItem item -> { model | newItem = item }
            AddItem ->
                 let newItem =
                      model.newItem
                      |> String.trim
                 in
                    if String.isEmpty newItem || (model.items |> List.any (\i -> i == model.newItem))
                    then model
                    else { model | items = newItem::model.items, newItem = "" }
            RemoveItem item ->
                { model | items = model.items |> List.filter (\i -> i /= item) }
    in
        ( newModel, Cmd.none )

---- VIEW ----
view : Model -> Html Msg
view model =
    let items =
         model.items |> List.map (\i -> p [ onClick (RemoveItem i) ] [ text i ]) |> List.reverse
    in
        div []
            [h1 [] [ text "Elm TODO" ]
            , Html.form [ onSubmit AddItem ] [
                input [ placeholder "Add new item", value model.newItem, onInput NewItem ] []
              ]
            , div [] items
            ,p [] [ text model.newItem ]
            ]
