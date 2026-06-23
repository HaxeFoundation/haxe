(*
	Module signatures — the *diff layer* of a module header (Phase 1 prototype, increment 1).

	A [module_signature] reduces a typed module's public, dependent-observable surface to per-entry
	canonical strings, keyed so the field-dependency edges (dep_field) map onto entries 1:1:
	  - a structural string per type declaration (the non-field signature), and
	  - a string per field, keyed "m:"/"s:"/"c:" (member/static/constructor), "e:" (enum constructor).

	This is ONLY the change-detection layer; it is not the header object itself (which is a module_def
	skeleton). Two signatures — including one rebuilt from a previous session — can be diffed entry by
	entry to decide which dependents observe a change.

	Impl-field bodies (inline/macro/@:generic) ARE dependent-observable (callers inline/specialize/eval
	them), so the *unoptimized* body is captured too, rendered with var ids normalized to positional so
	it depends only on body structure, not on names/ids that drift fresh-vs-restored.

	NOTE: type-printer determinism for monomorphs/lazies is not yet hardened, and the real
	fresh-vs-cache-restored check (server scenario) is still to come.
*)

open Globals
open Ast
open Type

(* [decl_signature] / [module_signature] are defined in TType so [module_def_extra] can hold one
   ([m_sig]); this module computes and diffs them. *)

(* ---------------------------------------------------------------------- *)
(* Leaf printers                                                          *)

let s_field_kind s_type = function
	| Method MethNormal -> "fun"
	| Method MethInline -> "inline fun"
	| Method MethDynamic -> "dynamic fun"
	| Method MethMacro -> "macro fun"
	| Var v -> Printf.sprintf "var(%s,%s)" (TPrinting.s_access true v.v_read) (TPrinting.s_access false v.v_write)

let observable_field_flags = [
	CfPublic,"public"; CfStatic,"static"; CfExtern,"extern"; CfFinal,"final";
	CfOverride,"override"; CfAbstract,"abstract"; CfOverload,"overload";
	CfImpl,"impl"; CfEnum,"enum"; CfGeneric,"generic";
]

let s_field_flags cf =
	String.concat "," (List.filter_map (fun (fl,name) ->
		if has_class_field_flag cf fl then Some name else None
	) observable_field_flags)

let observable_class_flags = [
	CExtern,"extern"; CFinal,"final"; CInterface,"interface"; CAbstract,"abstract";
]

let s_class_flags c =
	String.concat "," (List.filter_map (fun (fl,name) ->
		if has_class_flag c fl then Some name else None
	) observable_class_flags)

(* Optimization-derived meta (DCE @:used/@:directlyUsed/@:valueUsed, purity inference's
   @:pure(inferredPure)) depends on whole-program analysis and the compiler stage, so it would churn
   the signature — excluded. Everything else is rendered faithfully WITH ARGS: dropping args would be
   a false-negative (a meta whose args changed must register as a change). Sorted for determinism. *)
let is_derived_meta = function
	| (Meta.Used,_,_) | (Meta.DirectlyUsed,_,_) | (Meta.ValueUsed,_,_) -> true
	| (Meta.Pure,[(EConst (Ident "inferredPure"),_)],_) -> true
	| _ -> false

let s_meta meta =
	let meta = List.filter (fun m -> not (is_derived_meta m)) meta in
	match meta with
	| [] -> ""
	(* sort_uniq: deterministic order, and collapse metas that render identically (e.g. a source
	   @:value and the typer-injected one) — indistinguishable to a dependent anyway. *)
	| _ -> String.concat " " (List.sort_uniq compare (List.map (fun m -> TPrinting.Printer.s_metadata [m]) meta))

(* Implementation fields carry their body into callers, so the body is part of what dependents
   observe. Local var names/ids are not stable across compiles (dedup suffixes, generated-temp names,
   fresh id counters), so the rendered body's "name<id>" var tokens are rewritten to a positional
   "$n" by first appearance of each id — collision-free and dependent only on body structure. *)
let is_impl_field cf =
	match cf.cf_kind with
	| Method (MethInline | MethMacro) -> true
	| Var { v_read = AccInline } -> true
	| _ -> has_class_field_flag cf CfGeneric

let normalize_var_tokens s =
	let n = String.length s in
	let buf = Buffer.create n in
	let ids = Hashtbl.create 16 in
	let next = ref 0 in
	let is_name_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '`' in
	(* If "<digits>" starts at p, return (id, pos after '>'). *)
	let id_at p =
		if p < n && s.[p] = '<' then begin
			let k = ref (p + 1) in
			while !k < n && s.[!k] >= '0' && s.[!k] <= '9' do incr k done;
			if !k > p + 1 && !k < n && s.[!k] = '>' then Some (String.sub s (p + 1) (!k - p - 1), !k + 1) else None
		end else None
	in
	let i = ref 0 in
	while !i < n do
		let c = s.[!i] in
		if is_name_char c then begin
			let j = ref !i in
			while !j < n && is_name_char s.[!j] do incr j done;
			(match id_at !j with
			| Some (id,nx) ->
				let pos = try Hashtbl.find ids id with Not_found -> let p = !next in incr next; Hashtbl.add ids id p; p in
				Buffer.add_string buf (Printf.sprintf "$%d" pos);
				i := nx
			| None ->
				Buffer.add_string buf (String.sub s !i (!j - !i)); i := !j)
		end else begin
			Buffer.add_char buf c; incr i
		end
	done;
	Buffer.contents buf

let s_field_body s_type cf =
	if not (is_impl_field cf) then ""
	else
		let e = match cf.cf_expr_unoptimized with Some _ as u -> u | None -> cf.cf_expr in
		match e with
		| Some e -> normalize_var_tokens (TPrinting.s_expr_pretty true "" false s_type e)
		| None -> ""

let s_type_params s_type params =
	match params with
	| [] -> ""
	| _ -> "<" ^ String.concat "," (List.map (TPrinting.s_type_param s_type) params) ^ ">"

let s_inst s_type (c,tl) = s_type (TInst(c,tl))

(* ---------------------------------------------------------------------- *)
(* Field + declaration signatures                                         *)

let s_field s_type cf =
	Printf.sprintf "%s%s:%s|%s|%s|%s"
		(s_field_kind s_type cf.cf_kind)
		(s_type_params s_type cf.cf_params)
		(s_type cf.cf_type)
		(s_field_flags cf)
		(s_meta cf.cf_meta)
		(s_field_body s_type cf)

let sep = "\x1f"

let s_class_struct s_type c =
	String.concat sep [
		"class";
		(if c.cl_private then "priv" else "");
		s_class_flags c;
		s_type_params s_type c.cl_params;
		"super=" ^ (match c.cl_super with None -> "" | Some ct -> s_inst s_type ct);
		"impl=" ^ String.concat "," (List.map (s_inst s_type) c.cl_implements);
		"meta=" ^ s_meta c.cl_meta;
	]

let s_enum_struct s_type en =
	String.concat sep [
		"enum";
		(if en.e_private then "priv" else "");
		(if has_enum_flag en EnExtern then "extern" else "");
		s_type_params s_type en.e_params;
		"names=" ^ String.concat "," en.e_names;
		"meta=" ^ s_meta en.e_meta;
	]

let s_typedef_struct s_type td =
	String.concat sep [
		"typedef";
		(if td.t_private then "priv" else "");
		s_type_params s_type td.t_params;
		"type=" ^ s_type td.t_type;
		"meta=" ^ s_meta td.t_meta;
	]

let s_abstract_struct s_type a =
	String.concat sep [
		"abstract";
		(if a.a_private then "priv" else "");
		s_type_params s_type a.a_params;
		"this=" ^ s_type a.a_this;
		"from=" ^ String.concat "," (List.map s_type a.a_from);
		"to=" ^ String.concat "," (List.map s_type a.a_to);
		"impl=" ^ (match a.a_impl with None -> "" | Some c -> s_type_path c.cl_path);
		"meta=" ^ s_meta a.a_meta;
	]

let add_field prefix s_type cf fields =
	PMap.add (prefix ^ cf.cf_name) (s_field s_type cf) fields

let class_decl s_type c =
	let fields = PMap.empty in
	let fields = List.fold_left (fun acc cf -> add_field "m:" s_type cf acc) fields c.cl_ordered_fields in
	let fields = List.fold_left (fun acc cf -> add_field "s:" s_type cf acc) fields c.cl_ordered_statics in
	let fields = match c.cl_constructor with None -> fields | Some cf -> PMap.add "c:" (s_field s_type cf) fields in
	{ ds_struct = s_class_struct s_type c; ds_fields = fields }

let enum_decl s_type en =
	let fields = List.fold_left (fun acc name ->
		let ef = PMap.find name en.e_constrs in
		let sg = Printf.sprintf "%d|%s|%s|%s" ef.ef_index (s_type_params s_type ef.ef_params) (s_type ef.ef_type) (s_meta ef.ef_meta) in
		PMap.add ("e:" ^ name) sg acc
	) PMap.empty en.e_names in
	{ ds_struct = s_enum_struct s_type en; ds_fields = fields }

let decl_of_module_type s_type mt =
	let name = snd (t_path mt) in
	let decl = match mt with
		| TClassDecl c -> class_decl s_type c
		| TEnumDecl en -> enum_decl s_type en
		| TTypeDecl td -> { ds_struct = s_typedef_struct s_type td; ds_fields = PMap.empty }
		| TAbstractDecl a -> { ds_struct = s_abstract_struct s_type a; ds_fields = PMap.empty }
	in
	(name,decl)

let of_module m =
	let s_type = Type.s_type (Type.print_context()) in
	let decls = List.fold_left (fun acc mt ->
		let (name,decl) = decl_of_module_type s_type mt in
		PMap.add name decl acc
	) PMap.empty m.m_types in
	{ msig_decls = decls }

(* Compute and store the module's signature in m_extra. Called at cache time (pre-DCE, so the full
   public surface is present, and cf_expr_unoptimized exists for impl fields). Carried in mc_extra,
   so a cache-restored module already has it. *)
let compute_and_store m =
	m.m_extra.m_sig <- Some (of_module m)

(* ---------------------------------------------------------------------- *)
(* Diffing                                                                *)

type sig_change =
	| ScTypeAdded of string
	| ScTypeRemoved of string
	| ScStructural of string
	| ScFieldAdded of string * string
	| ScFieldRemoved of string * string
	| ScFieldChanged of string * string

let s_sig_change = function
	| ScTypeAdded s -> "+type " ^ s
	| ScTypeRemoved s -> "-type " ^ s
	| ScStructural s -> "~struct " ^ s
	| ScFieldAdded(t,f) -> Printf.sprintf "+field %s.%s" t f
	| ScFieldRemoved(t,f) -> Printf.sprintf "-field %s.%s" t f
	| ScFieldChanged(t,f) -> Printf.sprintf "~field %s.%s" t f

let diff_decl name old_d new_d acc =
	let acc = if old_d.ds_struct <> new_d.ds_struct then ScStructural name :: acc else acc in
	let acc = PMap.foldi (fun key new_sig acc ->
		match (try Some (PMap.find key old_d.ds_fields) with Not_found -> None) with
		| None -> ScFieldAdded(name,key) :: acc
		| Some old_sig -> if old_sig <> new_sig then ScFieldChanged(name,key) :: acc else acc
	) new_d.ds_fields acc in
	PMap.foldi (fun key _ acc ->
		if PMap.mem key new_d.ds_fields then acc else ScFieldRemoved(name,key) :: acc
	) old_d.ds_fields acc

let diff old_sig new_sig =
	let acc = PMap.foldi (fun name new_d acc ->
		match (try Some (PMap.find name old_sig.msig_decls) with Not_found -> None) with
		| None -> ScTypeAdded name :: acc
		| Some old_d -> diff_decl name old_d new_d acc
	) new_sig.msig_decls [] in
	PMap.foldi (fun name _ acc ->
		if PMap.mem name new_sig.msig_decls then acc else ScTypeRemoved name :: acc
	) old_sig.msig_decls acc

(* ---------------------------------------------------------------------- *)
(* Consuming a diff against a dependent's field-dependency edges           *)

(* The decl_signature field key a dep_field maps to. cl_init (CfrInit) is not part of the signature,
   so it has no key — the caller treats None as "any change to the declaring type". *)
let field_key_of_dep_field df =
	match df.dfd_kind with
	| CfrStatic -> Some ("s:" ^ df.dfd_field)
	| CfrMember -> Some ("m:" ^ df.dfd_field)
	| CfrConstructor -> Some "c:"
	| CfrInit -> None

(* Does a single dependency edge observe [changes] (the diff of the target module's signature)?
   - macro-origin edges are implementation dependencies (the dependent ran a macro from the target),
     which can observe anything the header does not model -> any change is observable.
   - field edge (dep_tgt = Some df): observable iff that field's own signature changed/was removed,
     or its declaring type's structure changed (which can shift what the field means).
   - module-level edge (dep_tgt = None: import, inheritance, structural reference): observes
     structural changes, type add/remove and field removals, but NOT individual field
     signature changes/additions. Those are caught by the field-granular edges — every real field
     use has one (invariant verified by -D hxb.verify_field_deps) — so relying on that keeps imports
     from invalidating on every edit. *)
let edge_observes_changes changes edge =
	match edge.dep_tgt_origin with
	| MDepFromMacro | MDepFromMacroDefine ->
		true
	| _ ->
	match edge.dep_tgt with
	| None ->
		List.exists (function
			| ScStructural _ | ScTypeAdded _ | ScTypeRemoved _ | ScFieldRemoved _ -> true
			| ScFieldChanged _ | ScFieldAdded _ -> false
		) changes
	| Some df ->
		let tn = snd df.dfd_path in
		let key = field_key_of_dep_field df in
		List.exists (function
			| ScStructural n | ScTypeRemoved n -> n = tn
			| ScTypeAdded _ -> false
			| ScFieldChanged(n,k) | ScFieldAdded(n,k) | ScFieldRemoved(n,k) ->
				n = tn && (match key with Some key -> key = k | None -> true)
		) changes

(* Given the target module's signature [changes] and the set of edges from a dependent that point at
   that target, does the dependent observe any change (=> it must be invalidated)? *)
let dependent_observes_changes changes edges =
	changes <> [] && List.exists (edge_observes_changes changes) edges

(* ---------------------------------------------------------------------- *)
(* Rendering (for the dump diagnostic)                                    *)

let render sg =
	let buf = Buffer.create 256 in
	let decls = List.sort (fun (a,_) (b,_) -> compare a b) (PMap.foldi (fun k v acc -> (k,v) :: acc) sg.msig_decls []) in
	List.iter (fun (name,decl) ->
		Buffer.add_string buf (Printf.sprintf "=== %s ===\n" name);
		Buffer.add_string buf (Printf.sprintf "  struct: %s\n" decl.ds_struct);
		let fields = List.sort (fun (a,_) (b,_) -> compare a b) (PMap.foldi (fun k v acc -> (k,v) :: acc) decl.ds_fields []) in
		List.iter (fun (k,v) -> Buffer.add_string buf (Printf.sprintf "  %s = %s\n" k v)) fields
	) decls;
	Buffer.contents buf
