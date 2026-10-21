(*
	Recording the dependencies of typing nodes (docs/incremental-compilation.md,
	§3.3 and phase 5).

	The consumer is the node being computed: the header or the members of a
	type, the signature or the body of a field (the queries of phase 3.1). What
	it reads through the accessors is recorded: the header or the members of a
	type, the signature or the inline body of a field, the lookup of a module.
	Nodes are named as -D dump-fingerprints names them: an entity key and a
	node kind ("header", "members", "sig", "inline", "body", "lookup").
	Recording is off unless asked for (-D dump-fingerprints for now).
*)
open Globals
open TType

type consumer = {
	c_key : string;
	c_node : string;
	(* the module of the consumer, and the name of its declaration in it
	   ("Type" or "Type.field", as -D dump-fingerprints names declarations) *)
	c_module : module_def;
	c_decl : string;
	(* the field computed, for the body of a field *)
	c_field : tclass_field option;
	(* where the node is declared, and the messages reported while computing it *)
	c_pos : pos;
	mutable c_messages : (string * int * pos) list;
	(* "key\tnode" of each dependency *)
	c_deps : (string,unit) Hashtbl.t;
}

let enabled = ref false

let current : consumer option DomainLocal.t = DomainLocal.make (fun () -> None)

let consumers : consumer list ref = ref []
let mutex = Mutex.create ()

let type_key path = s_type_path path

(* The name of the declaration of type [c]: the fields of the implementation
   of an abstract are declared in the abstract. *)
let type_decl c = match c.cl_kind with
	| KAbstractImpl a -> snd a.a_path
	| _ -> snd c.cl_path

let field_decl c cf =
	match c.cl_constructor with
	| Some cf' when cf' == cf -> type_decl c ^ ".new"
	| _ -> type_decl c ^ "." ^ cf.cf_name

(* The key of field [cf] of [c], as [EntityKey.to_string] prints it (main
   field, not an overload). *)
let field_key c cf =
	let base = s_type_path c.cl_path in
	match c.cl_constructor with
	| Some cf' when cf' == cf -> base ^ " (constructor)"
	| _ ->
		let static = cf.cf_flags land (1 lsl (Obj.magic CfStatic : int)) <> 0 in
		Printf.sprintf "%s.%s (%s)" base cf.cf_name (if static then "static" else "member")

let record key node =
	match DomainLocal.get current with
	| Some c -> Hashtbl.replace c.c_deps (key ^ "\t" ^ node) ()
	| None -> ()

(* The typedef of the statics of a type ([Class<T>], [Enum<T>], [Abstract<T>])
   is made of its members. *)
let record_type path node = if !enabled then match path with
	| [],name when String.length name > 2 && name.[String.length name - 1] = '>' && (match String.index_opt name '<' with Some i -> List.mem (String.sub name 0 i) ["Class";"Enum";"Abstract"] | None -> false) ->
		let i = String.index name '<' in
		record (String.sub name (i + 1) (String.length name - i - 2)) "members"
	| _ ->
		record (type_key path) node
let record_field c cf node = if !enabled then record (field_key c cf) node
let record_lookup path = if !enabled then record (s_type_path path) "lookup"

(* Runs [f] as the computation of node [node] of [key], declared as [decl] in module [m]. *)
(* A message reported (its text, severity and position): part of what the
   current node computes. *)
let record_message msg severity p =
	if !enabled then match DomainLocal.get current with
		| Some c -> c.c_messages <- (msg,severity,p) :: c.c_messages
		| None -> ()

let reset () = consumers := []

(* The modules typed by the last compilation of the server, for `server/retyped`. *)
let last_typed : string list ref = ref []

let with_consumer ?field key node m decl pos f =
	if not !enabled then f ()
	else begin
		let c = { c_key = key; c_node = node; c_module = m; c_decl = decl; c_field = field; c_pos = pos; c_messages = []; c_deps = Hashtbl.create 8 } in
		Mutex.protect mutex (fun () -> consumers := c :: !consumers);
		let old = DomainLocal.get current in
		DomainLocal.set current (Some c);
		Std.finally (fun () -> DomainLocal.set current old) f ()
	end
