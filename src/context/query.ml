(*
	Demand-driven typing queries (docs/incremental-compilation.md, phase 3.1).

	A query computes one piece of typing information about one entity (the
	header or the members of a type, the signature or the body of a field). A
	query is demanded when that information is needed, and computed at most
	once. Demanding a query that is running on the query stack is a cycle: it
	is always an error, reported with the chain of queries that leads to it
	(§3.3).
*)
open Globals

type kind =
	| Header of path
	| Members of path
	| Sig of path * string
	| Body of path * string

type state =
	| NotStarted
	| Running
	| Done
	| Failed

type t = {
	q_kind : kind;
	mutable q_state : state;
}

let make kind = { q_kind = kind; q_state = NotStarted }

let s_kind = function
	| Header path -> Printf.sprintf "header(%s)" (s_type_path path)
	| Members path -> Printf.sprintf "members(%s)" (s_type_path path)
	| Sig(path,name) -> Printf.sprintf "sig(%s.%s)" (s_type_path path) name
	| Body(path,name) -> Printf.sprintf "body(%s.%s)" (s_type_path path) name

(* The queries being computed, innermost first. Typing runs on one domain. *)
let stack : t list ref = ref []

let is_running q = q.q_state = Running

(* Tells whether a query of kind [kind] is running. *)
let is_kind_running kind = List.exists (fun q -> q.q_kind = kind) !stack

(* Raises the cycle error for demanding a query of kind [kind] that is running:
   the chain goes from that query to the innermost one, and back to it.
   [reason] says what demanded it. *)
let cycle_error kind reason p =
	let rec take acc = function
		| [] -> acc
		| q :: l -> if q.q_kind = kind then q :: acc else take (q :: acc) l
	in
	let chain = List.map (fun q -> s_kind q.q_kind) (take [] !stack) @ [s_kind kind] in
	Error.raise_typing_error (Printf.sprintf "Cycle detected while building types: %s (%s)" (String.concat " -> " chain) reason) p

(* Runs [f] as the computation of query [q]. *)
let run q f =
	let old = !stack in
	q.q_state <- Running;
	stack := q :: old;
	match f () with
	| v ->
		stack := old;
		q.q_state <- Done;
		v
	| exception e ->
		stack := old;
		q.q_state <- Failed;
		raise e
