module Main exposing (..)

import Components.Item as Item exposing (..)
import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, filter, head, concat, member, reverse)
import Debug exposing (log)


main : Program Never
main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { items : List IndexedItem
    , uid : Int
    }


type alias Id =
    Int


type alias IndexedItem =
    { id : Id
    , children : List Id
    , model : Item.Model
    }


init : Model
init =
    { items = []
    , uid = 0
    }



-- UPDATE


type Msg
    = AddParent
    | AddSibling
    | AddChild
    | Remove
    | Modify Id Item.Msg


update : Msg -> Model -> Model
update message ({ items, uid } as model) =
    let
        active =
            getId (getActive items)

        activeChildren =
            getChildren (getActive items)

        parent =
            getId (getParent active items)
    in
        case message of
            AddParent ->
                { model
                    | items =
                        [ IndexedItem uid
                            [ active ]
                            (Item.init ("New entity" ++ toString uid))
                        ]
                            ++ map (replaceChildren parent [ active ] [ uid ]) items
                    , uid = uid + 1
                }

            AddSibling ->
                { model
                    | items =
                        [ IndexedItem uid
                            []
                            (Item.init ("New entity" ++ toString uid))
                        ]
                            ++ map (addChildren parent [ uid ]) items
                    , uid = uid + 1
                }

            AddChild ->
                { model
                    | items =
                        [ IndexedItem uid
                            []
                            (Item.init ("New entity" ++ toString uid))
                        ]
                            ++ map (addChildren active [ uid ]) items
                    , uid = uid + 1
                }

            Remove ->
                { model
                    | items =
                        items
                            |> filter (\l -> not (l.id == active))
                            |> map (addChildren parent activeChildren)
                    , uid = uid + 1
                }

            Modify id msg ->
                { model | items = map (updateHelp id msg) items }


updateHelp : Id -> Item.Msg -> IndexedItem -> IndexedItem
updateHelp targetId msg { id, children, model } =
    IndexedItem id
        children
        (if targetId == id then
            Item.update msg model
         else
            { model | isActive = False }
        )



-- removeItem : Id -> List IndexedItem -> List IndexedItem
-- removeItem targetId items =


addChildren : Id -> List Id -> IndexedItem -> IndexedItem
addChildren targetId newChildren { id, children, model } =
    IndexedItem id
        (if targetId == id then
            children ++ newChildren
         else
            children
        )
        { model | isActive = False }


replaceChildren : Id -> List Id -> List Id -> IndexedItem -> IndexedItem
replaceChildren targetId oldChildren newChildren { id, children, model } =
    IndexedItem id
        (if targetId == id then
            exclude children oldChildren ++ newChildren
         else
            children
        )
        { model | isActive = False }


getActive : List IndexedItem -> Maybe IndexedItem
getActive items =
    items
        |> filter (\l -> l.model.isActive)
        |> head


getParent : Id -> List IndexedItem -> Maybe IndexedItem
getParent id items =
    items
        |> filter (\l -> member id l.children)
        |> head


getId : Maybe IndexedItem -> Id
getId item =
    case item of
        Nothing ->
            -1

        Just item ->
            item.id


getChildren : Maybe IndexedItem -> List Id
getChildren item =
    case item of
        Nothing ->
            []

        Just item ->
            item.children


exclude : List Id -> List Id -> List Id
exclude full part =
    filter (\l -> not (member l part)) full



-- VIEW


view : Model -> Html Msg
view model =
    let
        addParent =
            button [ onClick AddParent ] [ text "Add Parent" ]

        addSibling =
            button [ onClick AddSibling ] [ text "Add Sibling" ]

        addChild =
            button [ onClick AddChild ] [ text "Add Child" ]

        remove =
            button [ onClick Remove ] [ text "Remove" ]

        tree =
            viewBranches model.items (findRoots model.items)
    in
        div [] ([ addParent, addSibling, addChild, remove ] ++ [ div [ class "entities-menu" ] [ tree ] ])


findRoots : List IndexedItem -> List Id
findRoots items =
    let
        allIds =
            map (\l -> l.id) items

        allChildren =
            items
                |> map (\l -> l.children)
                |> concat
    in
        exclude allIds allChildren


viewBranches : List IndexedItem -> List Id -> Html Msg
viewBranches items ids =
    let
        selectedItems =
            filter (\l -> member l.id ids) items
    in
        case ids of
            [] ->
                text ""

            _ ->
                span [] (map (\l -> div [ class "level" ] [ App.map (Modify l.id) (Item.view l.model), viewBranches items l.children ]) (reverse selectedItems))
