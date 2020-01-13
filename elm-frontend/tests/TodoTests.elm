module TodoTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (describe, test, Test)
import Todo exposing (init, update, Msg(..))
import Tuple exposing (first)
import Expect exposing (equalLists)

suite : Test
suite =
    let defaultModel = init |> first in
    describe "Todo app"
        [test "Can add item" <| \_ ->
            update AddItem { defaultModel | newItem = "Some new item" }
            |> first
            |> .items
            |> equalLists ["Some new item"]
        ,test "Can't add identical items" <| \_ ->
            update AddItem { defaultModel | newItem = "Some item", items = [ "Some item" ] }
            |> first
            |> .items
            |> equalLists ["Some item"]
        ,test "Does not add empty item" <| \_ ->
            update AddItem defaultModel
            |> first
            |> .items
            |> equalLists []
        ,test "Does not add item containing only whitespaces" <| \_ ->
            update AddItem { defaultModel | newItem = "   " }
            |> first
            |> .items
            |> equalLists []
        ,test "Whitespaces are trimmed when adding" <| \_ ->
            update AddItem { defaultModel | newItem = "   Some item    " }
            |> first
            |> .items
            |> equalLists [ "Some item" ]
        ,test "Can remove item" <| \_ ->
            update (RemoveItem "Some item") { defaultModel | items = [ "Some item" ]}
            |> first
            |> .items
            |> equalLists []
        ]

