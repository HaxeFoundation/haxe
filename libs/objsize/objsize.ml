if Sys.ocaml_version < "3.11"
then
  failwith "Objsize >=0.12 can only be used with OCaml >=3.11"

type info =
  { data : int
  ; headers : int
  ; depth : int
  ; reached : bool
  }

let reach_set (roots : Obj.t list) =
  match roots with
  | [] -> 0
  | _ ->
    let n = List.length roots in
    let a = Array.of_list roots in
    (Obj.reachable_words (Obj.repr a)) - (n + 1)

let objsize obj (exclude:Obj.t list) (reach:Obj.t list) =
  let v = Obj.repr obj in
  let data = (reach_set (v :: exclude)) - (reach_set exclude) in
  let reached = match reach with
    | [] -> false
    | _ ->
      let only_v = reach_set [v] in
      let only_reach = reach_set reach in
      let both = reach_set (v :: reach) in
      both < only_v + only_reach
  in
  { data; headers = 0; depth = 0; reached }

let size_with_headers i = (Sys.word_size/8) * (i.data + i.headers)

let size_without_headers i = (Sys.word_size/8) * i.data
