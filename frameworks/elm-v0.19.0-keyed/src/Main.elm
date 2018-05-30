module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Attribute, Html, a, button, div, h1, span, table, td, text, tr)
import Html.Attributes exposing (attribute, class, classList, href, id, type_)
import Html.Events exposing (onClick)
import Html.Keyed
import Html.Lazy
import Random exposing (Generator, Seed)
import String


main : Program () Model Msg
main =
    Browser.embed
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }


adjectives : List String
adjectives =
    [ "pretty"
    , "large"
    , "big"
    , "small"
    , "tall"
    , "short"
    , "long"
    , "handsome"
    , "plain"
    , "quaint"
    , "clean"
    , "elegant"
    , "easy"
    , "angry"
    , "crazy"
    , "helpful"
    , "mushy"
    , "odd"
    , "unsightly"
    , "adorable"
    , "important"
    , "inexpensive"
    , "cheap"
    , "expensive"
    , "fancy"
    ]


colors : List String
colors =
    [ "red"
    , "yellow"
    , "blue"
    , "green"
    , "pink"
    , "brown"
    , "purple"
    , "brown"
    , "white"
    , "black"
    , "orange"
    ]


nouns : List String
nouns =
    [ "table"
    , "chair"
    , "house"
    , "bbq"
    , "desk"
    , "car"
    , "pony"
    , "cookie"
    , "sandwich"
    , "burger"
    , "pizza"
    , "mouse"
    , "keyboard"
    ]


buttons : List ( String, String, Msg )
buttons =
    [ ( "run", "Create 1,000 rows", Create 1000 )
    , ( "runlots", "Create 10,000 rows", Create 10000 )
    , ( "add", "Append 1,000 rows", Append 1000 )
    , ( "update", "Update every 10th row", UpdateEvery 10 )
    , ( "clear", "Clear", Clear )
    , ( "swaprows", "Swap Rows", Swap )
    ]


tbody : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
tbody attrs children =
    Html.Keyed.node "tbody" attrs children


btnPrimaryBlock : ( String, String, Msg ) -> Html Msg
btnPrimaryBlock ( buttonId, labelText, msg ) =
    div
        [ class "col-sm-6 smallpad" ]
        [ button
            [ type_ "button"
            , class "btn btn-primary btn-block"
            , id buttonId
            , onClick msg
            , attribute "ref" "text"
            ]
            [ text labelText ]
        ]


viewKeyedRow : Row -> ( String, Html Msg )
viewKeyedRow row =
    ( String.fromInt row.id, Html.Lazy.lazy viewRow row )


viewRow : Row -> Html Msg
viewRow { id, label, selected } =
    tr
        [ classList [ ( "danger", selected ) ] ]
        [ td [ class "col-md-1" ] [ text (String.fromInt id) ]
        , td
            [ class "col-md-4" ]
            [ a
                [ href "#"
                , onClick (Select id)
                ]
                [ text label ]
            ]
        , td
            [ class "col-md-1" ]
            [ a
                [ href "#"
                , onClick (Remove id)
                ]
                [ span
                    [ class "glyphicon glyphicon-remove"
                    , attribute "aria-hidden" "true"
                    ]
                    []
                ]
            ]
        , td [ class "col-md-6" ] []
        ]


view : Model -> Html Msg
view model =
    div
        [ class "container" ]
        [ div
            [ class "jumbotron" ]
            [ div
                [ class "row" ]
                [ div
                    [ class "col-md-6" ]
                    [ h1
                        []
                        [ text "Elm 0.18.0" ]
                    ]
                , div
                    [ class "col-md-6" ]
                    (List.map btnPrimaryBlock buttons)
                ]
            ]
        , table
            [ class "table table-hover table-striped test-data" ]
            [ tbody
                []
                (List.map viewKeyedRow model.rows)
            ]
        , span
            [ class "preloadicon glyphicon glyphicon-remove"
            , attribute "aria-hidden" "true"
            ]
            []
        ]


createRandomBatch : Maybe Seed -> Int -> Int -> ( List Row, Seed )
createRandomBatch maybeSeed amount lastId =
    case maybeSeed of
        Just seed ->
            let
                ( list, newSeed ) =
                    Random.step (batch amount) seed

                row =
                    createRow lastId
            in
            ( List.indexedMap row list, newSeed )

        Nothing ->
            ( [], Random.initialSeed 0 )


createRow : Int -> Int -> String -> Row
createRow lastId index label =
    { id = lastId + index
    , label = label
    , selected = False
    }


emptyRow : Row
emptyRow =
    Row 0 "" False


type Msg
    = Create Int
    | Append Int
    | UpdateEvery Int
    | Clear
    | Swap
    | Remove Int
    | Select Int
    | UpdateSeed Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create amount ->
            let
                ( newRows, seed ) =
                    createRandomBatch model.seed amount model.lastId
            in
            ( { model
                | rows = newRows
                , seed = Just seed
                , lastId = model.lastId + amount
              }
            , Cmd.none
            )

        Append amount ->
            let
                ( newRows, seed ) =
                    createRandomBatch model.seed amount model.lastId
            in
            ( { model
                | rows = model.rows ++ newRows
                , seed = Just seed
                , lastId = model.lastId + amount
              }
            , Cmd.none
            )

        UpdateEvery amount ->
            ( { model | rows = List.indexedMap updateRow model.rows }, Cmd.none )

        Clear ->
            ( { model | rows = [] }, Cmd.none )

        Swap ->
            if List.length model.rows > 998 then
                let
                    arr =
                        model.rows
                            |> Array.fromList

                    from =
                        arr |> Array.get 1 |> Maybe.withDefault emptyRow

                    to =
                        arr |> Array.get 998 |> Maybe.withDefault emptyRow
                in
                ( { model
                    | rows =
                        arr
                            |> Array.set 1 to
                            |> Array.set 998 from
                            |> Array.toList
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Remove id ->
            ( { model | rows = List.filter (\r -> r.id /= id) model.rows }, Cmd.none )

        Select id ->
            ( { model | rows = List.map (select id) model.rows }, Cmd.none )

        UpdateSeed seed ->
            ( { model | seed = Just seed }, Cmd.none )


updateRow : Int -> Row -> Row
updateRow index row =
    if (index |> modBy 10) == 0 then
        { row | label = row.label ++ " !!!" }

    else
        row


select : Int -> Row -> Row
select targetId ({ id, label, selected } as row) =
    if id == targetId then
        Row id label True

    else if selected == True then
        Row id label False

    else
        row


type alias Model =
    { seed : Maybe Seed
    , rows : List Row
    , lastId : Int
    }


type alias Row =
    { id : Int
    , label : String
    , selected : Bool
    }


init : a -> ( Model, Cmd Msg )
init _ =
    ( { seed = Nothing
      , rows = []
      , lastId = 1
      }
    , Random.generate UpdateSeed Random.independentSeed
    )


batch : Int -> Generator (List String)
batch n =
    Random.list n generator


generator : Generator String
generator =
    case ( adjectives, colors, nouns ) of
        ( a :: djectives, c :: olors, n :: ouns ) ->
            Random.map3
                (\a_ c_ n_ -> String.join " " [ a_, c_, n_ ])
                (Random.uniform a djectives)
                (Random.uniform c olors)
                (Random.uniform n ouns)

        _ ->
            Random.constant ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
