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

(* A dependency: an entity and the node read. Keys are only built when the
   dependencies are stored: recording allocates as little as possible (in
   parallel typing, allocation means minor collections that stop every
   domain). *)
type dep =
	| DType of path * string
	| DField of tclass * tclass_field * string
	| DKey of string * string
	(* a field looked up by name and not found in class [path] (a member, or
	   a static): only a field of that name added there changes the lookup *)
	| DAbsent of path * string * bool

module DepTbl = Hashtbl.Make(struct
	type t = dep
	let equal a b = match a,b with
		| DType(p1,n1),DType(p2,n2) -> n1 = n2 && p1 = p2
		| DField(c1,f1,n1),DField(c2,f2,n2) -> f1 == f2 && c1 == c2 && n1 = n2
		| DKey(k1,n1),DKey(k2,n2) -> n1 = n2 && k1 = k2
		| DAbsent(p1,n1,s1),DAbsent(p2,n2,s2) -> n1 = n2 && s1 = s2 && p1 = p2
		| _ -> false
	let hash = function
		| DType((_,name),n) -> Hashtbl.hash name * 31 + Hashtbl.hash n
		| DField(c,f,n) -> (Hashtbl.hash (snd c.cl_path) * 31 + Hashtbl.hash f.cf_name) * 31 + Hashtbl.hash n
		| DKey(k,n) -> Hashtbl.hash k * 31 + Hashtbl.hash n
		| DAbsent((_,name),n,_) -> Hashtbl.hash name * 31 + Hashtbl.hash n + 7
end)

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
	(* what it read *)
	c_deps : unit DepTbl.t;
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

let is_static cf = cf.cf_flags land (1 lsl (Obj.magic CfStatic : int)) <> 0

(* A member field cannot be named [new]: the constructor, or a copy of it (an
   inherited constructor, with the type parameters of the class applied). *)
let is_constructor c cf =
	match c.cl_constructor with
	| Some cf' when cf' == cf -> true
	| _ -> cf.cf_name = "new" && not (is_static cf)

let field_decl c cf =
	if is_constructor c cf then type_decl c ^ ".new"
	else type_decl c ^ "." ^ cf.cf_name

(* The key of field [cf] of [c], as [EntityKey.to_string] prints it (main
   field, not an overload). *)
let field_key c cf =
	let base = s_type_path c.cl_path in
	if is_constructor c cf then base ^ " (constructor)"
	else Printf.sprintf "%s.%s (%s)" base cf.cf_name (if is_static cf then "static" else "member")

(* The key of field [name] of the class at [path], as [field_key] gives it. *)
let field_key_of path name ~constructor ~static =
	let base = s_type_path path in
	if constructor then base ^ " (constructor)"
	else Printf.sprintf "%s.%s (%s)" base name (if static then "static" else "member")

let add d =
	match DomainLocal.get current with
	| Some c -> if not (DepTbl.mem c.c_deps d) then DepTbl.replace c.c_deps d ()
	| None -> ()

let record key node = if !enabled && DomainLocal.get current <> None then add (DKey(key,node))

(* The typedef of the statics of a type ([Class<T>], [Enum<T>], [Abstract<T>])
   is made of its members. Reading the statics of a class goes through
   [TAccess.Members] (a static found or not by name, all of them for a
   structural unification), so following [Class<T>] only reads the header of
   [T]. *)
let record_type path node = if !enabled && DomainLocal.get current <> None then match path with
	| [],name when String.length name > 2 && name.[String.length name - 1] = '>' && (match String.index_opt name '<' with Some i -> List.mem (String.sub name 0 i) ["Class";"Enum";"Abstract"] | None -> false) ->
		let i = String.index name '<' in
		let kind = String.sub name 0 i in
		add (DKey(String.sub name (i + 1) (String.length name - i - 2),if kind = "Class" then "header" else "members"))
	| _ ->
		add (DType(path,node))
let record_field c cf node = if !enabled && DomainLocal.get current <> None then add (DField(c,cf,node))
let record_lookup path = if !enabled && DomainLocal.get current <> None then add (DType(path,"lookup"))
let record_absent path name static = if !enabled && DomainLocal.get current <> None then add (DAbsent(path,name,static))

(* The node of an absence: "absent name" or "absent static name". *)
let absent_node name static = (if static then "absent static " else "absent ") ^ name

(* The key and the node of a dependency. *)
let dep_key = function
	| DType(path,node) -> type_key path,node
	| DField(cl,cf,node) -> field_key cl cf,node
	| DKey(key,node) -> key,node
	| DAbsent(path,name,static) -> type_key path,absent_node name static

(* The dependencies of [c], as keys and nodes. *)
let iter_deps f c =
	DepTbl.iter (fun d () -> let key,node = dep_key d in f key node) c.c_deps

let iter_raw_deps f c = DepTbl.iter (fun d () -> f d) c.c_deps

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
		let c = { c_key = key; c_node = node; c_module = m; c_decl = decl; c_field = field; c_pos = pos; c_messages = []; c_deps = DepTbl.create 8 } in
		Mutex.protect mutex (fun () -> consumers := c :: !consumers);
		let old = DomainLocal.get current in
		DomainLocal.set current (Some c);
		Std.finally (fun () -> DomainLocal.set current old) f ()
	end

(* [f], which reads for no computation (the creation of the types of a module
   from its declarations: the computation that demanded the module read its
   lookup, and reads what it uses of the types). *)
let untracked f =
	if not !enabled then f ()
	else begin
		let old = DomainLocal.get current in
		DomainLocal.set current None;
		Std.finally (fun () -> DomainLocal.set current old) f ()
	end

(* [f], to run later as part of the current computation: work a computation
   schedules (checks, connections, constraints) reads for it. *)
let capture f =
	if not !enabled then f
	else match DomainLocal.get current with
	| None -> f
	| Some _ as c ->
		(fun () ->
			let old = DomainLocal.get current in
			DomainLocal.set current c;
			Std.finally (fun () -> DomainLocal.set current old) f ())

(* Runs [f] and gives the dependencies it recorded (recorded for the current
   computation too), to record them again where the result is reused. *)
let collect f =
	match DomainLocal.get current with
	| Some c when !enabled ->
		let c' = { c with c_deps = DepTbl.create 8 } in
		DomainLocal.set current (Some c');
		let r = Std.finally (fun () -> DomainLocal.set current (Some c)) f () in
		let deps = DepTbl.fold (fun d () acc -> d :: acc) c'.c_deps [] in
		List.iter add deps;
		r,deps
	| _ ->
		f (),[]

let replay deps = List.iter add deps

(* The static variable whose initializer runs (macro context): the identity
   of a [haxe.macro.TypeCache] (phase 7). *)
let current_static : string option DomainLocal.t = DomainLocal.make (fun () -> None)
