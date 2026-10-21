(*
	State local to the domain running a computation (docs/incremental-compilation.md,
	phase 3.5): the stacks and the dynamically scoped settings of typing, so that
	field bodies can be typed on several domains.
*)

(* A reference local to each domain, initialized by [init] on first use. *)
type 'a t = 'a ref Domain.DLS.key

let make (init : unit -> 'a) : 'a t = Domain.DLS.new_key (fun () -> ref (init ()))

let get (r : 'a t) = !(Domain.DLS.get r)

let set (r : 'a t) v = (Domain.DLS.get r) := v

(* Values bound to keys (compared physically) for the dynamic extent of a
   computation, local to the domain running it. *)
module Scoped = struct
	type ('k,'v) t = ('k * 'v) list Domain.DLS.key

	let create () : ('k,'v) t = Domain.DLS.new_key (fun () -> [])

	let find (s : ('k,'v) t) (k : 'k) =
		let rec loop = function
			| [] -> None
			| (k',v) :: l -> if k' == k then Some v else loop l
		in
		loop (Domain.DLS.get s)

	(* Binds [k] to [v] until the returned function is called. *)
	let push (s : ('k,'v) t) (k : 'k) (v : 'v) =
		let l = Domain.DLS.get s in
		Domain.DLS.set s ((k,v) :: l);
		(fun () -> Domain.DLS.set s l)
end
