(*
	Canonical fingerprints of typed entities and parsed declarations
	(docs/incremental-compilation.md, §3.2 and phase 4).

	A fingerprint summarizes what consumers of an entity can observe. It does
	not depend on memory addresses: module types are identified by path, type
	parameters by their host and name, local variables by their order of
	declaration, unbound monomorphs by their order of appearance; anonymous
	structure fields are sorted by name. Positions are excluded from interface
	fingerprints (header, members, signature, inline body) and included in
	body and declaration fingerprints, relative to the position of the
	declaration: they do not change when the declaration moves in its file
	(they move with its anchor). Values are written directly into the digest.
*)
open Globals
open Ast
open Type

type t = int * int

(* The digest: two independent multiplicative lanes. *)
type state = {
	mutable h1 : int;
	mutable h2 : int;
	(* with positions: relative to this one in its file, absolute in other files *)
	positions : bool;
	mutable origin : pos;
	(* positions outside of the origin hash as their file only *)
	mutable within_origin : bool;
	(* the local variables declared so far, by id, and the unbound monomorphs *)
	mutable vars : int IntMap.t;
	mutable nvars : int;
	mutable monos : tmono list;
	(* the structures being hashed, against cycles through monomorphs *)
	mutable anons : tanon list;
	(* the flags of the local variables that are hashed *)
	mutable var_flags : int;
}

(* the flags of local variables that the typer or the analyzer set as markers
   (looked up, analyzed, written by hxb) are not hashed: they change when the
   expression is read again *)
let default_var_flags = lnot (List.fold_left (fun acc f -> acc lor (1 lsl (Obj.magic f : int))) 0 [VAnalyzed;VUsedByTyper;VHxb])

let create ~positions = {
	h1 = 0x2545F4914F6CDD1D;
	h2 = 0x1B873593C2B2AE35;
	positions;
	origin = null_pos;
	within_origin = false;
	vars = IntMap.empty;
	nvars = 0;
	monos = [];
	anons = [];
	var_flags = default_var_flags;
}

let add_int h x =
	let a = (h.h1 lxor x) * 0x100000001B3 in
	h.h1 <- a lxor (a lsr 29);
	let b = (h.h2 + x * 0x1E3779B97F4A7C15) * 0x32B2AE3D27D4EB4F in
	h.h2 <- b lxor (b lsr 31)

let add_bool h b = add_int h (if b then 1 else 0)

let add_string h s =
	add_int h (String.length s);
	let n = String.length s in
	let i = ref 0 in
	while !i + 8 <= n do
		add_int h (Int64.to_int (String.get_int64_le s !i));
		i := !i + 8
	done;
	while !i < n do
		add_int h (Char.code (String.unsafe_get s !i));
		incr i
	done

let finish h : t =
	let f x =
		let x = x lxor (x lsr 33) in
		let x = x * 0x3F51AFD7ED558CCD in
		x lxor (x lsr 29)
	in
	(f h.h1, f h.h2)

let to_string ((a,b) : t) = Printf.sprintf "%016x%016x" (a land max_int) (b land max_int)

(* A small tag before each kind of value, so that different shapes do not collide. *)
let tag h n = add_int h (n + 1_000_003)

let add_list h f l =
	add_int h (List.length l);
	List.iter (f h) l

let add_option h f = function
	| None -> tag h 0
	| Some v -> tag h 1; f h v

let add_path h (pack,name) =
	add_list h add_string pack;
	add_string h name

let add_pos h p =
	if h.positions then begin
		let file = Pos.file p in
		if h.within_origin && not (file = Pos.file h.origin && Pos.min p >= Pos.min h.origin && Pos.max p <= Pos.max h.origin) then
			add_string h file
		else if file = Pos.file h.origin then begin
			let origin = Pos.min h.origin in
			add_int h (Pos.min p - origin);
			add_int h (Pos.max p - origin)
		end else begin
			add_string h file;
			add_int h (Pos.min p);
			add_int h (Pos.max p)
		end
	end

(* Parsed data (expressions, metadata, declarations): a walk over its blocks,
   positions excepted. *)
let add_data h v =
	let rec walk v =
		if Obj.is_int v then begin
			tag h 1;
			add_int h (Obj.obj v : int)
		end else begin
			let t = Obj.tag v in
			if t = Obj.string_tag then begin
				tag h 2;
				add_string h (Obj.obj v : string)
			end else if t = Obj.double_tag then begin
				tag h 3;
				add_int h (Int64.to_int (Int64.bits_of_float (Obj.obj v : float)))
			end else if t = Obj.double_array_tag then begin
				tag h 4;
				let a : float array = Obj.obj v in
				Array.iter (fun f -> add_int h (Int64.to_int (Int64.bits_of_float f))) a
			end else if t < Obj.no_scan_tag && t <> Obj.closure_tag && t <> Obj.infix_tag && t <> Obj.lazy_tag && t <> Obj.forward_tag && t <> Obj.object_tag then begin
				match Pos.of_obj v with
				| Some p ->
					tag h 5;
					add_pos h p
				| None ->
					let size = Obj.size v in
					tag h 6;
					add_int h t;
					add_int h size;
					for i = 0 to size - 1 do walk (Obj.field v i) done
			end else
				tag h 7
		end
	in
	walk (Obj.repr v)

let add_meta h (meta : metadata) =
	add_list h (fun h (m,el,p) ->
		add_string h (Meta.to_string m);
		add_data h el;
		add_pos h p
	) meta

(* The flags that change what consumers see (not the markers of DCE and
   post-processing). *)
let class_flags c =
	List.fold_left (fun acc (flag,bit) -> if has_class_flag c flag then acc lor bit else acc) 0
		[CExtern,1;CFinal,2;CInterface,4;CAbstract,8;CFunctionalInterface,16]

let field_flags cf =
	List.fold_left (fun acc (flag,bit) -> if has_class_field_flag cf flag then acc lor bit else acc) 0
		[CfPublic,1;CfStatic,2;CfExtern,4;CfFinal,8;CfModifiesThis,16;CfOverride,32;CfAbstract,64;CfOverload,128;CfImpl,256;CfEnum,512;CfGeneric,1024;CfDefault,2048;CfNoLookup,4096;CfAbstractConstructor,8192]

let host_tag = function
	| TPHType -> 0
	| TPHConstructor -> 1
	| TPHMethod -> 2
	| TPHEnumConstructor -> 3
	| TPHAnonField -> 4
	| TPHLocal -> 5
	| TPHUnbound -> 6

let add_access h = function
	| AccNormal -> tag h 0
	| AccNo -> tag h 1
	| AccNever -> tag h 2
	| AccCtor -> tag h 3
	| AccCall -> tag h 4
	| AccPrivateCall -> tag h 5
	| AccInline -> tag h 6
	| AccRequire (s,so) -> tag h 7; add_string h s; add_option h add_string so

let add_field_kind h = function
	| Var v -> tag h 0; add_access h v.v_read; add_access h v.v_write
	| Method MethNormal -> tag h 1
	| Method MethInline -> tag h 2
	| Method MethDynamic -> tag h 3
	| Method MethMacro -> tag h 4

let rec add_type h t =
	match t with
	| TMono r ->
		begin match r.tm_type with
		| Some t -> add_type h t
		| None ->
			let rec index i = function
				| [] -> h.monos <- h.monos @ [r]; i
				| r' :: l -> if r' == r then i else index (i + 1) l
			in
			tag h 10;
			add_int h (index 0 h.monos)
		end
	| TLazy r ->
		begin match !r with
		| LAvailable t -> add_type h t
		| LProcessing _ | LWait _ -> tag h 11
		end
	| TEnum (en,tl) ->
		tag h 12; add_path h en.e_path; add_params h tl
	| TInst ({cl_kind = KTypeParameter ttp} as c,tl) ->
		tag h 13; add_int h (host_tag ttp.ttp_host); add_path h c.cl_path; add_params h tl
	| TInst (c,tl) ->
		tag h 14; add_path h c.cl_path; add_params h tl
	| TType (td,tl) ->
		tag h 15; add_path h td.t_path; add_params h tl
	| TAbstract (a,tl) ->
		tag h 16; add_path h a.a_path; add_params h tl
	| TFun (args,ret) ->
		tag h 17;
		add_list h (fun h (n,o,t) -> add_string h n; add_bool h o; add_type h t) args;
		add_type h ret
	| TDynamic None ->
		tag h 18
	| TDynamic (Some t) ->
		tag h 19; add_type h t
	| TAnon an ->
		add_anon h an

and add_params h tl = add_list h add_type tl

and add_anon h an =
	let rec index i = function
		| [] -> None
		| an' :: l -> if an' == an then Some i else index (i + 1) l
	in
	match index 0 h.anons with
	| Some i ->
		tag h 20; add_int h i
	| None ->
		match !(an.a_status) with
		| ClassStatics c -> tag h 21; add_path h c.cl_path
		| EnumStatics en -> tag h 22; add_path h en.e_path
		| AbstractStatics a -> tag h 23; add_path h a.a_path
		| status ->
			h.anons <- an :: h.anons;
			tag h 24;
			begin match status with
				| Closed -> tag h 0
				| Const -> tag h 1
				| Extend tl -> tag h 2; add_params h tl
				| _ -> ()
			end;
			let fields = List.sort (fun (n1,_) (n2,_) -> compare n1 n2) (PMap.foldi (fun n cf acc -> (n,cf) :: acc) an.a_fields []) in
			add_list h (fun h (n,cf) ->
				add_string h n;
				add_field_kind h cf.cf_kind;
				add_int h (field_flags cf);
				add_type_params h cf.cf_params;
				add_bool h (Meta.has Meta.Optional cf.cf_meta);
				add_type h cf.cf_type
			) fields;
			h.anons <- List.tl h.anons

and add_type_params h l =
	add_list h (fun h ttp ->
		add_string h ttp.ttp_name;
		add_int h (host_tag ttp.ttp_host);
		begin match ttp.ttp_constraints with
		| None -> tag h 0
		| Some l -> tag h 1; add_params h (AtomicLazy.force l)
		end;
		add_option h add_type ttp.ttp_default
	) l

(* Typed expressions *)

let declare_var h v =
	if not (IntMap.mem v.v_id h.vars) then begin
		h.vars <- IntMap.add v.v_id h.nvars h.vars;
		h.nvars <- h.nvars + 1
	end

let add_var_ref h v =
	declare_var h v;
	add_int h (IntMap.find v.v_id h.vars)

let add_var_decl h v =
	declare_var h v;
	add_var_ref h v;
	add_string h v.v_name;
	add_type h v.v_type;
	add_int h (v.v_flags land h.var_flags);
	add_meta h v.v_meta;
	add_pos h v.v_pos

let add_field_ref h c cf =
	add_path h c.cl_path;
	add_string h cf.cf_name

let add_module_type h mt = add_path h (t_infos mt).mt_path

let add_const h = function
	| TInt i -> tag h 0; add_int h (Int32.to_int i)
	| TFloat s -> tag h 1; add_string h s
	| TString s -> tag h 2; add_string h s
	| TBool b -> tag h 3; add_bool h b
	| TNull -> tag h 4
	| TThis -> tag h 5
	| TSuper -> tag h 6

let rec add_expr h e =
	add_pos h e.epos;
	add_type h e.etype;
	match e.eexpr with
	| TConst c -> tag h 30; add_const h c
	| TLocal v -> tag h 31; add_var_ref h v
	| TArray (e1,e2) -> tag h 32; add_expr h e1; add_expr h e2
	| TBinop (op,e1,e2) -> tag h 33; add_data h op; add_expr h e1; add_expr h e2
	| TField (e1,fa) ->
		tag h 34;
		add_expr h e1;
		begin match fa with
		| FInstance (c,tl,cf) -> tag h 0; add_field_ref h c cf; add_params h tl
		| FStatic (c,cf) -> tag h 1; add_field_ref h c cf
		| FAnon cf -> tag h 2; add_string h cf.cf_name
		| FDynamic s -> tag h 3; add_string h s
		| FClosure (None,cf) -> tag h 4; add_string h cf.cf_name
		| FClosure (Some (c,tl),cf) -> tag h 5; add_field_ref h c cf; add_params h tl
		| FEnum (en,ef) -> tag h 6; add_path h en.e_path; add_string h ef.ef_name
		end
	| TTypeExpr mt -> tag h 35; add_module_type h mt
	| TParenthesis e1 -> tag h 36; add_expr h e1
	| TObjectDecl fl ->
		tag h 37;
		add_list h (fun h ((n,p,qs),e) -> add_string h n; add_pos h p; add_data h qs; add_expr h e) fl
	| TArrayDecl el -> tag h 38; add_exprs h el
	| TCall (e1,el) -> tag h 39; add_expr h e1; add_exprs h el
	| TNew (c,tl,el) -> tag h 40; add_path h c.cl_path; add_params h tl; add_exprs h el
	| TUnop (op,flag,e1) -> tag h 41; add_data h op; add_data h flag; add_expr h e1
	| TFunction tf -> tag h 42; add_function h tf
	| TVar (v,eo) -> tag h 43; add_var_decl h v; add_option h add_expr eo
	| TBlock el -> tag h 44; add_exprs h el
	| TIf (e1,e2,eo) -> tag h 45; add_expr h e1; add_expr h e2; add_option h add_expr eo
	| TWhile (e1,e2,flag) -> tag h 46; add_data h flag; add_expr h e1; add_expr h e2
	| TSwitch sw ->
		tag h 47;
		add_expr h sw.switch_subject;
		add_list h (fun h case -> add_exprs h case.case_patterns; add_expr h case.case_expr) sw.switch_cases;
		add_option h add_expr sw.switch_default;
		add_bool h sw.switch_exhaustive
	| TTry (e1,catches) ->
		tag h 48;
		add_expr h e1;
		add_list h (fun h (v,e) -> add_var_decl h v; add_expr h e) catches
	| TReturn eo -> tag h 49; add_option h add_expr eo
	| TBreak -> tag h 50
	| TContinue -> tag h 51
	| TThrow e1 -> tag h 52; add_expr h e1
	| TCast (e1,mto) -> tag h 53; add_expr h e1; add_option h add_module_type mto
	| TMeta ((m,el,p),e1) -> tag h 54; add_string h (Meta.to_string m); add_data h el; add_pos h p; add_expr h e1
	| TEnumParameter (e1,ef,i) -> tag h 55; add_expr h e1; add_string h ef.ef_name; add_int h i
	| TEnumIndex e1 -> tag h 56; add_expr h e1
	| TIdent s -> tag h 57; add_string h s

and add_exprs h el = add_list h add_expr el

and add_function h tf =
	add_list h (fun h (v,eo) -> add_var_decl h v; add_option h add_expr eo) tf.tf_args;
	add_type h tf.tf_type;
	add_expr h tf.tf_expr

(* Fingerprints of the nodes of §3.2 *)

let with_state ?(origin=null_pos) ?(within_origin=false) ?(var_flags=default_var_flags) ~positions f =
	let h = create ~positions in
	h.origin <- origin;
	h.within_origin <- within_origin;
	h.var_flags <- var_flags;
	f h;
	finish h

let add_tinfos_header h (ti : tinfos) =
	add_path h ti.mt_path;
	add_bool h ti.mt_private;
	add_type_params h ti.mt_params;
	add_list h (fun h (c,_) -> add_path h c.cl_path) ti.mt_using

let add_class_kind h = function
	| KNormal -> tag h 0
	| KTypeParameter ttp -> tag h 1; add_int h (host_tag ttp.ttp_host)
	| KExpr _ -> tag h 2
	| KGeneric -> tag h 3
	| KGenericInstance (c,tl) -> tag h 4; add_path h c.cl_path; add_params h tl
	| KMacroType -> tag h 5
	| KGenericBuild _ -> tag h 6
	| KAbstractImpl a -> tag h 7; add_path h a.a_path
	| KModuleFields m -> tag h 8; add_path h m.m_path

(* [TypeHeader]: path, kind, parameters, super class and interfaces, declared
   metadata, [@:using], underlying type of an abstract, target of a typedef. *)
let type_header mt = with_state ~positions:false (fun h ->
	match mt with
	| TClassDecl c ->
		tag h 0;
		add_tinfos_header h (t_infos mt);
		add_class_kind h c.cl_kind;
		add_int h (class_flags c);
		add_option h (fun h (c,tl) -> add_path h c.cl_path; add_params h tl) c.cl_super;
		add_list h (fun h (c,tl) -> add_path h c.cl_path; add_params h tl) c.cl_implements;
		add_option h add_type c.cl_dynamic;
		add_option h add_type c.cl_array_access;
		add_meta h (match c.cl_header_meta with Some meta -> meta | None -> c.cl_meta)
	| TEnumDecl en ->
		tag h 1;
		add_tinfos_header h (t_infos mt);
		add_bool h (has_enum_flag en EnExtern);
		add_meta h en.e_meta
	| TTypeDecl td ->
		tag h 2;
		add_tinfos_header h (t_infos mt);
		add_meta h td.t_meta;
		add_type h td.t_type
	| TAbstractDecl a ->
		tag h 3;
		add_tinfos_header h (t_infos mt);
		add_bool h a.a_extern;
		add_bool h a.a_enum;
		add_meta h a.a_meta;
		add_type h a.a_this
)

let add_member h cf =
	add_string h cf.cf_name;
	add_field_kind h cf.cf_kind;
	add_int h (field_flags cf);
	add_int h (List.length cf.cf_overloads)

(* [TypeMembers]: the members after build macros (names, kinds, access), the
   complete metadata, the [from]/[to] and operators of an abstract. *)
let type_members mt = with_state ~positions:false (fun h ->
	match mt with
	| TClassDecl c ->
		tag h 0;
		add_meta h c.cl_meta;
		add_list h add_member c.cl_ordered_statics;
		add_list h add_member c.cl_ordered_fields;
		add_option h add_member c.cl_constructor;
		add_bool h (c.cl_init <> None)
	| TEnumDecl en ->
		tag h 1;
		add_list h (fun h name ->
			let ef = PMap.find name en.e_constrs in
			add_string h name;
			add_int h ef.ef_index;
			add_type_params h ef.ef_params;
			add_type h ef.ef_type;
			add_meta h ef.ef_meta
		) en.e_names
	| TTypeDecl _ ->
		tag h 2
	| TAbstractDecl a ->
		tag h 3;
		add_option h (fun h c -> add_path h c.cl_path) a.a_impl;
		add_params h a.a_from;
		add_list h (fun h (t,cf) -> add_type h t; add_string h cf.cf_name) a.a_from_field;
		add_params h a.a_to;
		add_list h (fun h (t,cf) -> add_type h t; add_string h cf.cf_name) a.a_to_field;
		add_list h (fun h (op,cf) -> add_data h op; add_string h cf.cf_name) a.a_ops;
		add_list h (fun h (op,flag,cf) -> add_data h op; add_data h flag; add_string h cf.cf_name) a.a_unops;
		add_list h (fun h cf -> add_string h cf.cf_name) a.a_array;
		List.iter (add_option h (fun h cf -> add_string h cf.cf_name)) [a.a_read;a.a_write;a.a_call;a.a_constructor]
)

let rec add_field_sig h cf =
	add_string h cf.cf_name;
	add_field_kind h cf.cf_kind;
	add_int h (field_flags cf);
	add_type_params h cf.cf_params;
	add_meta h cf.cf_meta;
	add_type h cf.cf_type;
	add_list h add_field_sig cf.cf_overloads

(* [FieldSig]: the type, kind, parameters, metadata and flags of a field (and
   of its overloads). *)
let field_sig cf = with_state ~positions:false (fun h -> add_field_sig h cf)

(* Whether consumers read the body of [cf]: inline fields, macros, generic
   fields, the constructors of [@:structInit] classes and the casts and
   operators of an abstract. *)
let has_inline_body c cf =
	match cf.cf_kind with
	| Method (MethInline | MethMacro) | Var { v_read = AccInline } -> true
	| _ when has_class_field_flag cf CfGeneric -> true
	(* the default values of the arguments of the constructor of a
	   [@:structInit] class are copied where a structure declares an instance *)
	| _ when (match c.cl_constructor with Some cf' -> cf' == cf | None -> false) && Meta.has Meta.StructInit c.cl_meta -> true
	| _ -> (match c.cl_kind with KAbstractImpl _ -> Meta.has Meta.From cf.cf_meta || Meta.has Meta.To cf.cf_meta || Meta.has Meta.Op cf.cf_meta | _ -> false)

let add_body h cf = add_option h add_expr cf.cf_expr

(* A structure, as the generators type it: its fields, their kinds and their
   types. *)
let anon an = with_state ~positions:false (fun h -> add_anon h an)

(* [FieldInlineBody]: the body of a field its consumers read. Its positions
   end up in their code (default values, [haxe.PosInfos], debug information
   of the targets keeping inlined positions): they are hashed relative to the
   field, like in [field_body]. A field that only moves keeps its
   fingerprint, its anchor moves the positions copied into its consumers; a
   field whose positions change within it (its lines end differently, code
   added before an expression) is read again by them. *)
let field_inline_body cf = with_state ~origin:cf.cf_pos ~within_origin:true ~positions:true (fun h -> add_body h cf)

(* [FieldBody]: the typed body, with positions relative to the field's: they
   move with the field (its anchor). The positions of the code inlined from
   other declarations move with these declarations (their anchors): only their
   file is hashed, what the code is comes from the inline bodies read. *)
let field_body cf = with_state ~origin:cf.cf_pos ~within_origin:true ~positions:true (fun h -> add_body h cf)

(* [Decls]: a parsed declaration (a field, or a type without its fields) at
   position [p], with positions relative to it. *)
let decl p v = with_state ~origin:p ~positions:true (fun h -> add_data h v)

(* Whether [e] contains line numbers of its file ([haxe.PosInfos]): they are
   constants of the typed body until positions are resolved after typing
   (docs/incremental-compilation.md, phase 2, item 6, and phase 8). *)
let uses_line_numbers e =
	let rec loop e = match e.eexpr with
		| TObjectDecl fl when List.exists (fun ((n,_,_),_) -> n = "lineNumber") fl -> raise Exit
		| _ -> Type.iter loop e
	in
	try loop e; false with Exit -> true
