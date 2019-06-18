(* Information gathered while walking through values. *)
type info =
  { data : int
  ; headers : int
  ; depth : int
  ; reached : bool
  }

(* Returns information for first argument, excluding the second arg list and telling if we can reach the third arg list *)
val objsize : 'a -> Obj.t list -> Obj.t list -> info

(* Calculates sizes in bytes: *)
val size_with_headers : info -> int
val size_without_headers : info -> int
