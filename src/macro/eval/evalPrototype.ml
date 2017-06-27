(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Globals
open Type
open EvalValue
open EvalContext
open EvalEncode
open EvalHash
open EvalJit
open EvalJitContext
open EvalExceptions
open EvalMisc

(* JITs expression [e] and executes the result immediately. *)
let eval_expr ctx key name e =
	catch_exceptions ctx (fun () ->
		let jit,f = jit_expr ctx e in
		let num_captures = Hashtbl.length jit.captures in
		let info = create_env_info true (file_hash e.epos.pfile) (EKMethod(key,name)) jit.capture_infos in
		let env = push_environment ctx info jit.max_num_locals num_captures in
		Std.finally (fun _ -> pop_environment ctx env) f env
	) e.Type.epos

(* Creates constructor function for class [c], if it has a constructor. *)
let create_constructor ctx c =
	match c.cl_constructor with
	| Some {cf_expr = Some {eexpr = TFunction tf; epos = pos}} when not c.cl_extern ->
		let key = path_hash c.cl_path in
		let v = lazy (vfunction (jit_tfunction ctx key key_new tf false pos)) in
		ctx.constructors <- IntMap.add key v ctx.constructors;
	| _ ->
		()

(*
	PrototypeBuilder manages the intermediate state of prototypes. Due to mutual dependencies,
	prototypes have to be created in multiple steps similar to how types have to be loaded.

	The procedure is as follows:

	1. Create an instance of [prototype_context] using [create].
	2. Add fields to it and call [finalize].
	3. Invoke the function returned by [finalize] to initialize fields.
*)
module PrototypeBuilder = struct
	type prototype_context = {
		ctx : context;
		(* The hashed path of the prototype. *)
		key : int;
		(* The parent prototype, if exists. *)
		parent : vprototype option;
		(* The kind of the prototype. *)
		kind : vprototype_kind;
		(* The fields of the prototype. A field here is a pair of its hashed name and its (lazyfied) value. *)
		fields : (int * value Lazy.t) DynArray.t;
		(* The instance fields of the prototype. See above. *)
		instance_fields : (int * value Lazy.t) DynArray.t;
		(* The metadata expression, if exists. *)
		meta : texpr option;
		(* Whether or not the prototype is static. *)
		is_static : bool;
	}

	(* Creates a new prototype context using the provided information. *)
	let create ctx key parent kind meta =
		let is_static = match kind with PClass _ | PEnum _ -> true | PInstance | PObject -> false in
		{
			ctx = ctx;
			key = key;
			parent = parent;
			kind = kind;
			fields = DynArray.create ();
			instance_fields = DynArray.create ();
			meta = meta;
			is_static = is_static;
		}

	(* Adds a prototype (static) field. *)
	let add_proto_field pctx name v =
		DynArray.add pctx.fields (name,v)

	(* Adds an instance (non-static) field. *)
	let add_instance_field pctx name v =
		DynArray.add pctx.instance_fields (name,v)

	(* Forces the lazy field values and assigns them to the prototype. *)
	let initialize_fields pctx proto =
		DynArray.iteri (fun i (_,v) -> proto.pfields.(i) <- Lazy.force v) pctx.fields

	(* Processes the field information and returns an initialization function. *)
	let finalize pctx =
		let ctx = pctx.ctx in
		(* Add builtins (from EvalStdLib). *)
		let builtins = try IntMap.find pctx.key (if pctx.is_static then ctx.builtins.static_builtins else ctx.builtins.instance_builtins) with Not_found -> [] in
		List.iter (fun (s,v) ->
			try
				let i = DynArray.index_of (fun (name',_) -> name' = s) pctx.fields in
				DynArray.set pctx.fields i (s,(lazy v))
			with Not_found ->
				add_proto_field pctx s (lazy v)
		) builtins;
		(* Add metadata field *)
		begin match pctx.meta with
			| None -> ()
			| Some e -> DynArray.add pctx.fields (key___meta__,lazy (match eval_expr ctx pctx.key key___meta__ e with Some e -> e | None -> vnull))
		end;
		(* Create the mapping from hashed name to field offset for prototype fields. *)
		let _,pnames = DynArray.fold_left (fun (i,acc) (name,_) -> i + 1,IntMap.add name i acc) (0,IntMap.empty) pctx.fields in
		let pinstance_names,pinstance_fields,f = if not pctx.is_static then begin
			(* Combines parent instance fields with current instance fields. *)
			let names,fields = match pctx.parent with
				| Some proto -> proto.pinstance_names,proto.pinstance_fields
				| None -> IntMap.empty,[||]
			in
			let a = Array.make (Array.length fields + DynArray.length pctx.instance_fields) vnull in
			Array.blit fields 0 a 0 (Array.length fields);
			(* Create the mapping from hashed name to field offset for instance fields. *)
			let names,_ = DynArray.fold_left (fun (fields,count) (name,v) ->
				IntMap.add name count fields,count + 1
			) (names,Array.length fields) pctx.instance_fields in
			names,a,(fun proto ->
				DynArray.iteri (fun i (_,v) -> a.(i + Array.length fields) <- Lazy.force v) pctx.instance_fields;
				initialize_fields pctx proto;
			)
		end else
			IntMap.empty,[||],(fun proto ->
				initialize_fields pctx proto;
			)
		in
		(* Create the prototype. *)
		let proto = {
			ppath = pctx.key;
			pfields = Array.make (DynArray.length pctx.fields) vnull;
			pnames = pnames;
			pinstance_fields = pinstance_fields;
			pinstance_names = pinstance_names;
			pparent = pctx.parent;
			pkind = pctx.kind;
			pvalue = vnull;
		} in
		proto.pvalue <- vprototype proto;
		(* Register the prototype. *)
		if pctx.is_static then
			ctx.static_prototypes <- IntMap.add pctx.key proto ctx.static_prototypes
		else begin
			ctx.instance_prototypes <- IntMap.add pctx.key proto ctx.instance_prototypes;
			if pctx.key = key_String then ctx.string_prototype <- proto;
		end;
		proto,f
end

let is_removable_field cf =
	Meta.has Meta.Extern cf.cf_meta || Meta.has Meta.Generic cf.cf_meta

let create_static_prototype ctx mt =
	let key = path_hash (t_infos mt).mt_path in
	let com = ctx.curapi.MacroApi.get_com() in
	let meta = Codegen.build_metadata com mt in
	let o = match mt with
	| TClassDecl c ->
		let pparent = match c.cl_super with
			| None -> None
			| Some(csup,_) -> Some (get_static_prototype ctx (path_hash csup.cl_path) c.cl_pos)
		in
		let interfaces = List.map (fun (c,_) -> path_hash c.cl_path) c.cl_implements in
		let pctx = PrototypeBuilder.create ctx key pparent (PClass interfaces) meta in
		let fields = List.filter (fun cf -> not (is_removable_field cf)) c.cl_ordered_statics in
		let delays = DynArray.create() in
		if not c.cl_extern then List.iter (fun cf -> match cf.cf_kind,cf.cf_expr with
			| Method _,Some {eexpr = TFunction tf; epos = pos} ->
				let name = hash_s cf.cf_name in
				PrototypeBuilder.add_proto_field pctx name (lazy (vstatic_function (jit_tfunction ctx key name tf true pos)));
			| Var _,Some e ->
				let name = hash_s cf.cf_name in
				PrototypeBuilder.add_proto_field pctx name (lazy vnull);
				let i = DynArray.length pctx.PrototypeBuilder.fields - 1 in
				DynArray.add delays (fun proto -> proto.pfields.(i) <- (match eval_expr ctx key name e with Some e -> e | None -> vnull))
			| _,None when is_physical_field cf ->
				PrototypeBuilder.add_proto_field pctx (hash_s cf.cf_name) (lazy vnull);
			|  _ ->
				()
		) fields;
		begin match c.cl_init with
			| None -> ()
			| Some e -> DynArray.add delays (fun _ -> ignore(eval_expr ctx key key___init__ e))
		end;
		PrototypeBuilder.finalize pctx,(DynArray.to_list delays)
	| TEnumDecl en ->
		let pctx = PrototypeBuilder.create ctx key None (PEnum en.e_names) meta in
		let enum_field_value ef = match follow ef.ef_type with
			| TFun(args,_) ->
				let f = match args with
					| [] -> Fun0 (fun () -> encode_enum_value key ef.ef_index [||] (Some ef.ef_pos))
					| [_] -> Fun1 (fun a -> encode_enum_value key ef.ef_index [|a|] (Some ef.ef_pos))
					| [_;_] -> Fun2 (fun a b -> encode_enum_value key ef.ef_index [|a;b|] (Some ef.ef_pos))
					| [_;_;_] -> Fun3 (fun a b c -> encode_enum_value key ef.ef_index [|a;b;c|] (Some ef.ef_pos))
					| [_;_;_;_] -> Fun4 (fun a b c d -> encode_enum_value key ef.ef_index [|a;b;c;d|] (Some ef.ef_pos))
					| [_;_;_;_;_] -> Fun5 (fun a b c d e -> encode_enum_value key ef.ef_index [|a;b;c;d;e|] (Some ef.ef_pos))
					| _ -> FunN (fun vl -> encode_enum_value key ef.ef_index (Array.of_list vl) (Some ef.ef_pos))
				in
				vstatic_function f
			| _ -> encode_enum_value key ef.ef_index [||] (Some ef.ef_pos)
		in
		PMap.iter (fun name ef -> PrototypeBuilder.add_proto_field pctx (hash_s name ) (lazy (enum_field_value ef))) en.e_constrs;
		PrototypeBuilder.finalize pctx,[];
	| TAbstractDecl a ->
		let pctx = PrototypeBuilder.create ctx key None (PClass []) meta in
		PrototypeBuilder.finalize pctx,[];
	| _ ->
		assert false
	in
	o

let create_instance_prototype ctx c =
	let pparent = match c.cl_super with
		| None -> None
		| Some(c,_) -> Some (get_instance_prototype ctx (path_hash c.cl_path) c.cl_pos)
	in
	let key = path_hash c.cl_path in
	let pctx = PrototypeBuilder.create ctx key pparent PInstance None in
	let fields = List.filter (fun cf -> not (is_removable_field cf)) c.cl_ordered_fields in
	if c.cl_extern && c.cl_path <> ([],"String") then
		()
	else List.iter (fun cf -> match cf.cf_kind,cf.cf_expr with
		| Method meth,Some {eexpr = TFunction tf; epos = pos} ->
			let name = hash_s cf.cf_name in
			let v = lazy (vfunction (jit_tfunction ctx key name tf false pos)) in
			if meth = MethDynamic then PrototypeBuilder.add_instance_field pctx name v;
			PrototypeBuilder.add_proto_field pctx name v
		| Var _,_ when is_physical_field cf ->
			let name = hash_s cf.cf_name in
			PrototypeBuilder.add_instance_field pctx name (lazy vnull);
		|  _ ->
			()
	) fields;
	PrototypeBuilder.finalize pctx

let get_object_prototype ctx l =
	let l = List.sort (fun (i1,_) (i2,_) -> if i1 = i2 then 0 else if i1 < i2 then -1 else 1) l in
	let sfields = String.concat "," (List.map (fun (i,_) -> rev_hash_s i) l) in
	let key = Hashtbl.hash sfields in
	try
		IntMap.find key ctx.instance_prototypes,l
	with Not_found ->
		let name = hash_s (Printf.sprintf "eval.object.Object[%s]" sfields) in
		let pctx = PrototypeBuilder.create ctx name None PObject None in
		List.iter (fun (name,_) -> PrototypeBuilder.add_instance_field pctx name (lazy vnull)) l;
		let proto = fst (PrototypeBuilder.finalize pctx) in
		ctx.instance_prototypes <- IntMap.add key proto ctx.instance_prototypes;
		proto,l

let add_types ctx types ready =
	let t = Common.timer [(if ctx.is_macro then "macro" else "interp");"add_types"] in
	let new_types = List.filter (fun mt ->
		let inf = Type.t_infos mt in
		let key = path_hash inf.mt_path in
		try
			let inf' = t_infos (IntMap.find key ctx.type_cache) in
			if inf'.mt_module.m_id <> inf.mt_module.m_id then raise Not_found;
			false
		with Not_found ->
			ctx.instance_prototypes <- IntMap.remove key ctx.instance_prototypes;
			ctx.static_prototypes <- IntMap.remove key ctx.static_prototypes;
			ctx.constructors <- IntMap.remove key ctx.constructors;
			ready mt;
			ctx.type_cache <- IntMap.add key mt ctx.type_cache;
			if ctx.debug.support_debugger then begin
				let file_key = hash_s inf.mt_module.m_extra.m_file in
				if not (Hashtbl.mem ctx.debug.breakpoints file_key) then begin
					Hashtbl.add ctx.debug.breakpoints file_key (Hashtbl.create 0)
				end
			end;
			true
	) types in
	(* 1. Create prototypes and register them. *)
	let fl_instance = DynArray.create () in
	let fl_static = DynArray.create () in
	List.iter (fun mt ->
		match mt with
		| TClassDecl c ->
			let rec loop p f =
				match p with
				| Some (p,_) when PMap.mem f.cf_name p.cl_fields || loop p.cl_super f ->
					Hashtbl.add ctx.overrides (p.cl_path,f.cf_name) true;
					true
				| _ ->
					false
			in
			List.iter (fun f -> ignore(loop c.cl_super f)) c.cl_overrides;
			create_constructor ctx c;
			DynArray.add fl_instance (create_instance_prototype ctx c);
			DynArray.add fl_static (create_static_prototype ctx mt);
		| TEnumDecl en ->
			DynArray.add fl_static (create_static_prototype ctx mt);
		| TAbstractDecl a ->
			DynArray.add fl_static (create_static_prototype ctx mt);
		| _ ->
			()
	) new_types;
	(* 2. Create instance fields. *)
	DynArray.iter (fun (proto,f) -> ignore(f proto)) fl_instance;
	(* 3. Create static fields. *)
	let fl_static_init = DynArray.create () in
	DynArray.iter (fun ((proto,f),delays) ->
		f proto;
		match delays with
		| [] -> ()
		| _ -> DynArray.add fl_static_init (proto,delays)
	) fl_static;
	(* 4. Initialize static fields. *)
	DynArray.iter (fun (proto,delays) -> List.iter (fun f -> f proto) delays) fl_static_init;
	t()