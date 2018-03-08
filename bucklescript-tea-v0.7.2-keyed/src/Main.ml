open Tea.App
open Tea.Html

let adjectives =
  [ "pretty"
  ; "large"
  ; "big"
  ; "small"
  ; "tall"
  ; "short"
  ; "long"
  ; "handsome"
  ; "plain"
  ; "quaint"
  ; "clean"
  ; "elegant"
  ; "easy"
  ; "angry"
  ; "crazy"
  ; "helpful"
  ; "mushy"
  ; "odd"
  ; "unsightly"
  ; "adorable"
  ; "important"
  ; "inexpensive"
  ; "cheap"
  ; "expensive"
  ; "fancy" ]


let colours =
  [ "red"
  ; "yellow"
  ; "blue"
  ; "green"
  ; "pink"
  ; "brown"
  ; "purple"
  ; "brown"
  ; "white"
  ; "black"
  ; "orange" ]


let nouns =
  [ "table"
  ; "chair"
  ; "house"
  ; "bbq"
  ; "desk"
  ; "car"
  ; "pony"
  ; "cookie"
  ; "sandwich"
  ; "burger"
  ; "pizza"
  ; "mouse"
  ; "keyboard" ]


type row = {id: int; label: string}

type model =
  {rows: row list; nextId: int; selected: int option; seed: Tea.Random.seed}

type msg =
  | Create of int
  | Append of int
  | UpdateEvery of int
  | Clear
  | Swap
  | Remove of int
  | Select of int
  [@@bs.deriving {accessors}]

let buttons =
  [ ("run", "Create 1,000 rows", Create 1000)
  ; ("runlots", "Create 10,000 rows", Create 10000)
  ; ("add", "Append 1,000 rows", Append 1000)
  ; ("update", "Update every 10th row", UpdateEvery 10)
  ; ("clear", "Clear", Clear)
  ; ("swaprows", "Swap Rows", Swap) ]


let init () =
  {rows= []; nextId= 0; seed= Tea.Random.initialSeed 0; selected= None}


let sample list =
  Tea.Random.int 0 (Belt.List.length list - 1)
  |> Tea.Random.map (fun i -> Belt.List.getExn list i)


let create_rows amount nextId =
  let next = ref (nextId + amount) in
  Tea.Random.list amount
    (Tea.Random.map3
       (fun x y z ->
         next := !next - 1 ;
         {id= !next + 1; label= x ^ " " ^ y ^ " " ^ z} )
       (sample adjectives) (sample colours) (sample nouns))
  |> Tea.Random.step


let rec update model = function
  | Create count -> update {model with rows= []} (Append count)
  | Append count ->
      let rows, seed = create_rows count model.nextId model.seed in
      { model with
        nextId= model.nextId + count
      ; rows= Belt.List.concat model.rows rows
      ; seed }
  | UpdateEvery n ->
      let rows =
        Belt.List.mapWithIndex model.rows (fun i row ->
            if i mod n = 0 then {row with label= row.label ^ " !!!"} else row
        )
      in
      {model with rows}
  | Clear -> {model with rows= []}
  | Swap ->
      let rows =
        match Belt.List.splitAt model.rows 998 with
        | Some (a :: b :: before, c :: after) ->
            Belt.List.concat (a :: c :: before) (b :: after)
        | _ -> model.rows
      in
      {model with rows}
  | Remove id ->
      let rows = Belt.List.keep model.rows (fun row -> row.id <> id) in
      {model with rows}
  | Select id -> {model with selected= Some id}


let btnPrimaryBlock (buttonId, labelText, msg) =
  div [class' "col-sm-6 smallpad"]
    [ button
        [ type' "button"
        ; class' "btn btn-primary btn-block"
        ; id buttonId
        ; onClick msg ]
        [text labelText] ]


let viewRow selected {id; label} =
  tr
    (* key *)
    [classList [("danger", Some id = selected)]]
    [ td [class' "col-md-1"] [text (string_of_int id)]
    ; td [class' "col-md-4"] [a [href "#"; onClick (Select id)] [text label]]
    ; td [class' "col-md-1"]
        [ a [href "#"; onClick (Remove id)]
            [ span
                [ class' "glyphicon glyphicon-remove"
                ; Vdom.attribute "" "aria-hidden" "true" ]
                [] ] ]
    ; td [class' "col-md-6"] [] ]


let view model =
  div [class' "container"]
    [ div [class' "jumbotron"]
        [ div [class' "row"]
            [ div [class' "col-md-6"] [h1 [] [text "BuckleScript-TEA 0.7.1"]]
            ; div [class' "col-md-6"] (Belt.List.map buttons btnPrimaryBlock)
            ] ]
    ; table
        [class' "table table-hover table-striped test-data"]
        [tbody [] (Belt.List.map model.rows (viewRow model.selected))]
    ; span
        [ class' "preloadicon glyphicon glyphicon-remove"
        ; Vdom.attribute "" "aria-hidden" "true" ]
        [] ]


let main = beginnerProgram {model= init (); update; view}
