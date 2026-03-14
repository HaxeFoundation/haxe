if Sys.ocaml_version < "3.11"
then
  failwith "Objsize >=0.12 can only be used with OCaml >=3.11"

type info =
  { data : int
  ; headers : int
  ; depth : int
  ; reached : bool
  }

let objsize obj (_exclude:Obj.t list) (_reach:Obj.t list) =
  (* The exclude and reach parameters are part of the public API but are not
     used by the OCaml implementation, which uses Obj.reachable_words instead
     of the original C ml_objsize stub. *)
  {data = (Obj.reachable_words (Obj.repr obj)); headers = 0; depth = 0; reached = false}

let size_with_headers i = (Sys.word_size/8) * (i.data + i.headers)

let size_without_headers i = (Sys.word_size/8) * i.data

