(*
	The Haxe Compiler
	Copyright (C) 2005-2019  Haxe Foundation

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
open Option
open Common
open Ast
open Type
open Codegen
open Texpr.Builder
open Gencommon

(* ******************************************* *)
(* Type Parameters *)
(* ******************************************* *)
(*
	This module will handle type parameters. There are lots of changes we need to do to correctly support type parameters:

	traverse will:
		V Detect when parameterized function calls are made
		* Detect when a parameterized class instance is being cast to another parameter
		* Change new<> parameterized function calls
		*

	extras:
		* On languages that support "real" type parameters, a Cast function is provided that will convert from a <Dynamic> to the requested type.
			This cast will call createEmpty with the correct type, and then set each variable to the new form. Some types will be handled specially, namely the Native Array.
			Other implementations may be delegated to the runtime.
		* parameterized classes will implement a new interface (with only a Cast<> function added to it), so we can access the <Dynamic> type parameter for them. Also any reference to <Dynamic> will be replaced by a reference to this interface. (also on TTypeExpr - Std.is())
		* Type parameter renaming to avoid name clash
		* Detect type parameter casting and call Cast<> instead

	for java:
		* for specially assigned classes, parameters will be replaced by _d and _i versions of parameterized functions. This will only work for parameterized classes, not functions.

	dependencies:
		must run after casts are detected. This will be ensured at CastDetect module.
*)
(* ******************************************* *)
(* Real Type Parameters Module *)
(* ******************************************* *)
(*
	This submodule is by now specially made for the .NET platform. There might be other targets that will
	make use of this, but it IS very specific.

	On the .NET platform, generics are real specialized classes that are JIT compiled. For this reason, we cannot
	cast from one type parameter to another. Also there is no common type for the type parameters, so for example
	an instance of type Array<Int> will return false for instance is Array<object> .

	So we need to:
		1. create a common interface (without type parameters) (e.g. "Array") which will only contain a __Cast<> function, which will cast from one type into another
		2. Implement the __Cast function. This part is a little hard, as we must identify all type parameter-dependent fields contained in the class and convert them.
		In most cases the conversion will just be to call .__Cast<>() on the instances, or just a simple cast. But when the instance is a @:nativegen type, there will be no .__Cast
		function, and we will need to deal with this case either at compile-time (added handlers - specially for NativeArray), or at runtime (adding new runtime handlers)
		3. traverse the AST looking for casts involving type parameters, and replace them with .__Cast<>() calls. If type is @:nativegen, throw a warning. If really casting from one type parameter to another on a @:nativegen context, throw an error.


	special literals:
		it will use the special literal __typehandle__ that the target must implement in order to run this. This literal is a way to get the typehandle of e.g. the type parameters,
		so we can compare them. In C# it's the equivalent of typeof(T).TypeHandle (TypeHandle compare is faster than System.Type.Equals())

	dependencies:
		(module filter) Interface creation must run AFTER enums are converted into classes, otherwise there is no way to tell parameterized enums to implement an interface
		Must run AFTER CastDetect. This will be ensured per CastDetect

*)
let name = "real_type_params"
let priority = max_dep -. 20.

let rec has_type_params t =
	match follow t with
		| TInst( { cl_kind = KTypeParameter _ }, _) -> true
		| TAbstract(_, params)
		| TEnum(_, params)
		| TInst(_, params) -> List.exists (fun t -> has_type_params t) params
		| TFun(args,ret) ->
			List.exists (fun (n,o,t) -> has_type_params t) args || has_type_params ret
		| _ -> false

let rec follow_all_md md =
	let t = match md with
		| TClassDecl { cl_kind = KAbstractImpl a } ->
			TAbstract(a, List.map snd a.a_params)
		| TClassDecl c ->
			TInst(c, List.map snd c.cl_params)
		| TEnumDecl e ->
			TEnum(e, List.map snd e.e_params)
		| TTypeDecl t ->
			TType(t, List.map snd t.t_params)
		| TAbstractDecl a ->
			TAbstract(a, List.map snd a.a_params)
	in
	Abstract.follow_with_abstracts t

let rec is_hxgeneric md =
	match md with
	| TClassDecl { cl_kind = KAbstractImpl a } ->
		is_hxgeneric (TAbstractDecl a)
	| TClassDecl(cl) ->
		not (Meta.has Meta.NativeGeneric cl.cl_meta)
	| TEnumDecl(e) ->
		not (Meta.has Meta.NativeGeneric e.e_meta)
	| TAbstractDecl(a) when Meta.has Meta.NativeGeneric a.a_meta ->
		not (Meta.has Meta.NativeGeneric a.a_meta)
	| md -> match follow_all_md md with
		| TInst(cl,_) -> is_hxgeneric (TClassDecl cl)
		| TEnum(e,_) -> is_hxgeneric (TEnumDecl e)
		| TAbstract(a,_) -> not (Meta.has Meta.NativeGeneric a.a_meta)
		| _ -> true

type nativegeneric_reason =
	| ReasonField of string * Type.t
	| ReasonSuper of Globals.path
	| ReasonExplicit

exception Cannot_be_native of Globals.path * pos * Globals.path * nativegeneric_reason

let rec set_hxgeneric gen mds isfirst md =
	let iface_path, raise_pos, raise_if_native = match md with
		| TClassDecl(cl) -> (try
			(fst (List.find (fun (cl,_) -> (set_hxgeneric gen mds isfirst (TClassDecl cl) ) = Some(true) ) cl.cl_implements)).cl_path, cl.cl_pos, true
		with Not_found ->
			([],""), Globals.null_pos, false)
		| _ -> ([],""), Globals.null_pos, false
	in
	let path = t_path md in
	if List.exists (fun m -> path = t_path m) mds then begin
		if isfirst then
			None (* we still can't determine *)
		else
			Some true (* if we're in second pass and still can't determine, it's because it can be hxgeneric *)
	end else begin
		let has_unresolved = ref false in
		let is_false v =
			match v with
				| Some false -> true
				| None -> has_unresolved := true; false
				| Some true -> false
		in
		let mds = md :: mds in
		match md with
			| TClassDecl(cl)	->
				(* first see if any meta is present (already processed) *)
				if Meta.has Meta.NativeGeneric cl.cl_meta then begin
					if raise_if_native then raise (Cannot_be_native(path, raise_pos, iface_path, ReasonExplicit));
					Some false
				end else if Meta.has Meta.HaxeGeneric cl.cl_meta then
					Some true
				else if cl.cl_params = [] && is_hxgen md then
					(cl.cl_meta <- (Meta.HaxeGeneric,[],cl.cl_pos) :: cl.cl_meta;
					Some true)
				else if cl.cl_params = [] then
					(cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
					Some false)
				else if not (is_hxgen md) then
					(cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
					Some false)
				else begin
					(*
						if it's not present, see if any superclass is nativegeneric.
						nativegeneric is inherited, while hxgeneric can be later changed to nativegeneric
					*)
					(* on the first pass, our job is to find any evidence that makes it not be hxgeneric. Otherwise it will be hxgeneric *)
					match cl.cl_super with
						| Some (c,_) when is_false (set_hxgeneric gen mds isfirst (TClassDecl c)) ->
							if raise_if_native then raise (Cannot_be_native(path, raise_pos, iface_path, ReasonSuper(c.cl_path)));
							cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
							Some false
						| _ ->
							(* see if it's a generic class *)
							match cl.cl_params with
								| [] ->
									(* if it's not, then it will follow hxgen *)
									if is_hxgen (TClassDecl cl) then
										cl.cl_meta <- (Meta.HaxeGeneric, [], cl.cl_pos) :: cl.cl_meta
									else
										cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
									Some true
								| _ ->
									(* if it is, loop through all fields + statics and look for non-hxgeneric
										generic classes that have KTypeParameter as params *)
									let raise_or_return_true = if raise_if_native then
											(fun cf -> raise (Cannot_be_native(path, raise_pos, iface_path, ReasonField(cf.cf_name, cf.cf_type))))
										else
											(fun cf -> true)
									in
									let rec cfs_must_be_native cfs =
										match cfs with
											| [] -> false
											| cf :: cfs when Type.is_physical_field cf ->
												let t = follow (gen.greal_type cf.cf_type) in
												(match t with
													| TInst( { cl_kind = KTypeParameter _ }, _ ) -> cfs_must_be_native cfs
													| TInst(cl,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TClassDecl cl)) ->
														if not (Hashtbl.mem gen.gtparam_cast cl.cl_path) then raise_or_return_true cf else cfs_must_be_native cfs
													| TEnum(e,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TEnumDecl e)) ->
														if not (Hashtbl.mem gen.gtparam_cast e.e_path) then raise_or_return_true cf else cfs_must_be_native cfs
													| _ -> cfs_must_be_native cfs (* TAbstracts / Dynamics can't be generic *)
												)
											| _ :: cfs ->
												cfs_must_be_native cfs
									in
									if cfs_must_be_native cl.cl_ordered_fields then begin
										cl.cl_meta <- (Meta.NativeGeneric, [], cl.cl_pos) :: cl.cl_meta;
										Some false
									end else if isfirst && !has_unresolved then
										None
									else begin
										cl.cl_meta <- (Meta.HaxeGeneric, [], cl.cl_pos) :: cl.cl_meta;
										Some true
									end
				end
			| TEnumDecl e ->
				if Meta.has Meta.NativeGeneric e.e_meta then begin
					if raise_if_native then raise (Cannot_be_native(path, raise_pos, iface_path, ReasonExplicit));
					Some false
				end else if Meta.has Meta.HaxeGeneric e.e_meta then
					Some true
				else if not (is_hxgen (TEnumDecl e)) then begin
					e.e_meta <- (Meta.NativeGeneric, [], e.e_pos) :: e.e_meta;
					Some false
				end else begin
					(* if enum is not generic, then it's hxgeneric *)
					match e.e_params with
						| [] ->
							e.e_meta <- (Meta.HaxeGeneric, [], e.e_pos) :: e.e_meta;
							Some true
						| _ ->
							let raise_or_return_true = if raise_if_native then
									(fun name t -> raise (Cannot_be_native(path, raise_pos, iface_path, ReasonField(name, t))))
								else
									(fun _ _ -> true)
							in
							let rec efs_must_be_native efs =
								match efs with
									| [] -> false
									| ef :: efs ->
										let t = follow (gen.greal_type ef.ef_type) in
										match t with
											| TFun(args, _) ->
												if List.exists (fun (n,o,t) ->
													let t = follow t in
													match t with
														| TInst( { cl_kind = KTypeParameter _ }, _ ) ->
															false
														| TInst(cl,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TClassDecl cl)) ->
															if not (Hashtbl.mem gen.gtparam_cast cl.cl_path) then raise_or_return_true ef.ef_name t else false
														| TEnum(e,p) when has_type_params t && is_false (set_hxgeneric gen mds isfirst (TEnumDecl e)) ->
															if not (Hashtbl.mem gen.gtparam_cast e.e_path) then raise_or_return_true ef.ef_name t else false
														| _ -> false
												) args then
													true
												else
													efs_must_be_native efs
											| _ -> efs_must_be_native efs
							in
							let efs = PMap.fold (fun ef acc -> ef :: acc) e.e_constrs [] in
							if efs_must_be_native efs then begin
								e.e_meta <- (Meta.NativeGeneric, [], e.e_pos) :: e.e_meta;
								Some false
							end else if isfirst && !has_unresolved then
								None
							else begin
								e.e_meta <- (Meta.HaxeGeneric, [], e.e_pos) :: e.e_meta;
								Some true
							end
				end
			| _ -> Globals.die "" __LOC__
	end

let path_s = function
	| [],name -> name
	| pack,name -> String.concat "." pack ^ "." ^ name

let set_hxgeneric gen md =
	try
		let ret = match md with
			| TClassDecl { cl_kind = KAbstractImpl a } -> (match follow_all_md md with
				| (TInst _ | TEnum _ as t) -> (
					let md = match t with
						| TInst(cl,_) -> TClassDecl cl
						| TEnum(e,_) -> TEnumDecl e
						| _ -> Globals.die "" __LOC__
					in
					let ret = set_hxgeneric gen [] true md in
					if ret = None then get (set_hxgeneric gen [] false md) else get ret)
				| TAbstract(a,_) -> true
				| _ -> true)
			| _ -> match set_hxgeneric gen [] true md with
				| None ->
					get (set_hxgeneric gen [] false md)
				| Some v ->
					v
		in
		if not ret then begin
			match md with
			| TClassDecl c ->
				let set_hxgeneric (_,param) = match follow param with
					| TInst(c,_) ->
						c.cl_meta <- (Meta.NativeGeneric, [], c.cl_pos) :: c.cl_meta
					| _ -> ()
				in
				List.iter set_hxgeneric c.cl_params;
				let rec handle_field cf =
					List.iter set_hxgeneric cf.cf_params;
					List.iter handle_field cf.cf_overloads
				in
				(match c.cl_kind with
					| KAbstractImpl a ->
						List.iter set_hxgeneric a.a_params;
					| _ -> ());
				List.iter handle_field c.cl_ordered_fields;
				List.iter handle_field c.cl_ordered_statics
			| _ -> ()
		end;
		ret
	with Cannot_be_native(path, pos, iface_path, reason) ->
		let reason_start = "The class at path " ^ path_s path ^ " implements a haxe generic interface " ^ path_s iface_path
			^ ". It however cannot be a haxe generic class "
		in
		let reason = reason_start ^ match reason with
			| ReasonField (field_name, t) ->
				"because its field " ^ field_name ^ " is of type " ^ debug_type t
			| ReasonSuper (path) ->
				"because it extends the type " ^ path_s path ^ " that was determined to be a native generic type"
			| ReasonExplicit ->
				"because it explicitly has the metadata @:nativeGeneric set"
		in
		gen.gcon.error (reason) pos;
		Globals.die "" __LOC__

let params_has_tparams params =
	List.fold_left (fun acc t -> acc || has_type_params t) false params

(* ******************************************* *)
(* RealTypeParamsModf *)
(* ******************************************* *)

(*

	This is the module filter of Real Type Parameters. It will traverse through all types and look for hxgeneric classes (only classes).
	When found, a parameterless interface will be created and associated via the "ifaces" Hashtbl to the original class.
	Also a "cast" function will be automatically generated which will handle unsafe downcasts to more specific type parameters (necessary for serialization)

	dependencies:
		Anything that may create hxgeneric classes must run before it.
		Should run before ReflectionCFs (this dependency will be added to ReflectionCFs), so the added interfaces also get to be real IHxObject's

*)

module RealTypeParamsModf =
struct

	let set_only_hxgeneric gen =
		let rec run md =
			match md with
				| TTypeDecl _ | TAbstractDecl _ -> md
				| _ -> ignore (set_hxgeneric gen md); md
		in
		run

	let name = "real_type_params_modf"

	let priority = solve_deps name []

	let rec get_fields gen cl params_cl params_cf acc =
		let fields = List.fold_left (fun acc cf ->
			match follow (gen.greal_type (gen.gfollow#run_f (cf.cf_type))) with
				| TInst(cli, ((_ :: _) as p)) when (not (is_hxgeneric (TClassDecl cli))) && params_has_tparams p ->
					(cf, apply_params cl.cl_params params_cl cf.cf_type, apply_params cl.cl_params params_cf cf.cf_type) :: acc
				| TEnum(e, ((_ :: _) as p)) when not (is_hxgeneric (TEnumDecl e)) && params_has_tparams p ->
					(cf, apply_params cl.cl_params params_cl cf.cf_type, apply_params cl.cl_params params_cf cf.cf_type) :: acc
				| _ -> acc
		) [] cl.cl_ordered_fields in
		match cl.cl_super with
			| Some(cs, tls) ->
				get_fields gen cs (List.map (apply_params cl.cl_params params_cl) tls) (List.map (apply_params cl.cl_params params_cf) tls) (fields @ acc)
			| None -> (fields @ acc)

	let get_cast_name cl = String.concat "_" ((fst cl.cl_path) @ [snd cl.cl_path; "cast"]) (* explicitly define it *)

	(* overrides all needed cast functions from super classes / interfaces to call the new cast function *)
	let create_stub_casts gen cl cast_cfield =
		(* go through superclasses and interfaces *)
		let p = cl.cl_pos in
		let this = { eexpr = TConst TThis; etype = (TInst(cl, List.map snd cl.cl_params)); epos = p } in

		let rec loop curcls params level reverse_params =
			if (level <> 0 || curcls.cl_interface) && params <> [] && is_hxgeneric (TClassDecl curcls) then begin
				let cparams = List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) curcls.cl_params in
				let name = get_cast_name curcls in
				if not (PMap.mem name cl.cl_fields) then begin
					let reverse_params = List.map (apply_params curcls.cl_params params) reverse_params in
					let cfield = mk_class_field name (TFun([], t_dynamic)) false cl.cl_pos (Method MethNormal) cparams in
					let field = { eexpr = TField(this, FInstance(cl,List.map snd cl.cl_params, cast_cfield)); etype = apply_params cast_cfield.cf_params reverse_params cast_cfield.cf_type; epos = p } in
					let call =
					{
						eexpr = TCall(field, []);
						etype = t_dynamic;
						epos = p;
					} in
					let call = gen.gparam_func_call call field reverse_params [] in
					let delay () =
						cfield.cf_expr <-
						Some {
							eexpr = TFunction(
							{
								tf_args = [];
								tf_type = t_dynamic;
								tf_expr = mk_return call
							});
							etype = cfield.cf_type;
							epos = p;
						}
					in
					gen.gafter_filters_ended <- delay :: gen.gafter_filters_ended; (* do not let filters alter this expression content *)
					cl.cl_ordered_fields <- cfield :: cl.cl_ordered_fields;
					cl.cl_fields <- PMap.add cfield.cf_name cfield cl.cl_fields;
					if level <> 0 then add_class_field_flag cfield CfOverride
				end
			end;
			let get_reverse super supertl =
				List.map (apply_params super.cl_params supertl) reverse_params
			in
			(match curcls.cl_super with
			| None -> ()
			| Some(super, supertl) ->
				let super_params = List.map (apply_params curcls.cl_params params) supertl in
				loop super (super_params) (level + 1) (get_reverse super super_params));
			List.iter (fun (iface, ifacetl) ->
				let iface_params = List.map (apply_params curcls.cl_params params) ifacetl in
				loop iface (iface_params) level (get_reverse iface iface_params);
			) curcls.cl_implements
		in
		loop cl (List.map snd cl.cl_params) 0 (List.map snd cl.cl_params)

	(*
		Creates a cast classfield, with the desired name

		Will also look for previous cast() definitions and override them, to reflect the current type and fields

		FIXME: this function still doesn't support generics that extend generics, and are cast as one of its subclasses. This needs to be taken care, by
		looking at previous superclasses and whenever a generic class is found, its cast argument must be overridden. the toughest part is to know how to type
		the current type correctly.
	*)
	let create_cast_cfield gen cl name =
		reset_temps();
		let basic = gen.gcon.basic in
		let cparams = List.map (fun (s,t) -> (s, TInst (map_param (get_cl_t t), []))) cl.cl_params in
		let cfield = mk_class_field name (TFun([], t_dynamic)) false cl.cl_pos (Method MethNormal) cparams in
		let params = List.map snd cparams in

		let fields = get_fields gen cl (List.map snd cl.cl_params) params [] in
		let fields = List.filter (fun (cf,_,_) -> Type.is_physical_field cf) fields in

		(* now create the contents of the function *)
		(*
			it will look something like:
			if (typeof(T) == typeof(T2)) return this;

			var new_me = new CurrentClass<T2>(EmptyInstnace);

			for (field in Reflect.fields(this))
			{
				switch(field)
				{
					case "aNativeArray":
						var newArray = new NativeArray(this.aNativeArray.Length);

					default:
						Reflect.setField(new_me, field, Reflect.field(this, field));
				}
			}
		*)
		let pos = cl.cl_pos in

		let new_me_var = alloc_var "new_me" (TInst (cl, params)) in
		let local_new_me = mk_local new_me_var pos in
		let this = mk (TConst TThis) (TInst (cl, List.map snd cl.cl_params)) pos in
		let field_var = alloc_var "field" basic.tstring in
		let local_field = mk_local field_var pos in
		let i_var = alloc_var "i" basic.tint in
		let local_i = mk_local i_var pos in
		let incr_i = mk (TUnop (Increment, Postfix, local_i)) basic.tint pos in
		let fields_var = alloc_var "fields" (basic.tarray basic.tstring) in
		let local_fields = mk_local fields_var pos in

		let fields_to_cases fields =
			let get_path t =
				match follow t with
				| TInst (cl,_) -> cl.cl_path
				| TEnum (e,_) -> e.e_path
				| TAbstract (a,_) -> a.a_path
				| TMono _ | TDynamic _ -> ([], "Dynamic")
				| _ -> Globals.die "" __LOC__
			in
			List.map (fun (cf, t_cl, t_cf) ->
				let t_cf = follow (gen.greal_type t_cf) in
				let this_field = mk (TField (this, FInstance (cl, List.map snd cl.cl_params, cf))) t_cl pos in
				let expr =
					binop
						OpAssign
						(mk (TField (local_new_me, FInstance(cl, List.map snd cl.cl_params, cf))) t_cf pos)
						(try (Hashtbl.find gen.gtparam_cast (get_path t_cf)) this_field t_cf with Not_found ->
							(* if not found tparam cast, it shouldn't be a valid hxgeneric *)
							print_endline ("Could not find a gtparam_cast for " ^ (String.concat "." (fst (get_path t_cf)) ^ "." ^ (snd (get_path t_cf))));
							Globals.die "" __LOC__)
						t_cf
						pos
				in
				[make_string gen.gcon.basic cf.cf_name pos], expr
			) fields
		in

		let mk_typehandle =
			(fun cl -> mk (TCall (mk (TIdent "__typeof__") t_dynamic pos, [make_static_this cl pos])) t_dynamic pos)
		in
		let mk_eq cl1 cl2 =
			binop OpEq (mk_typehandle cl1) (mk_typehandle cl2) basic.tbool pos
		in
		let rec mk_typehandle_cond thisparams cfparams =
			match thisparams, cfparams with
			| TInst (cl_this,[]) :: [], TInst (cl_cf,[]) :: [] ->
				mk_eq cl_this cl_cf
			| TInst (cl_this,[]) :: hd, TInst (cl_cf,[]) :: hd2 ->
				binop OpBoolAnd (mk_eq cl_this cl_cf) (mk_typehandle_cond hd hd2) basic.tbool pos
			| v :: hd, v2 :: hd2 ->
				(match follow v, follow v2 with
				| (TInst(cl1,[]) as v), (TInst(cl2,[]) as v2) ->
					mk_typehandle_cond (v :: hd) (v2 :: hd2)
				| _ ->
					Globals.die "" __LOC__)
			| _ -> Globals.die "" __LOC__
		in
		let fn = {
			tf_args = [];
			tf_type = t_dynamic;
			tf_expr = mk (TBlock [
				(* if (typeof(T) == typeof(T2)) return this *)
				mk (TIf (mk_typehandle_cond (List.map snd cl.cl_params) params, mk_return this, None)) basic.tvoid pos;
				(* var new_me = /*special create empty with tparams construct*/ *)
				mk (TVar (new_me_var, Some (gen.gtools.r_create_empty cl params pos))) basic.tvoid pos;
				(* var fields = Reflect.fields(this); *)
				mk (TVar (fields_var, Some (gen.gtools.r_fields true this))) basic.tvoid pos;
				(* var i = 0; *)
				mk (TVar (i_var, Some (make_int gen.gcon.basic 0 pos))) basic.tvoid pos;
				(* while (i < fields.length) *)
				mk (TWhile (
					binop OpLt local_i (mk_field_access gen local_fields "length" pos) basic.tbool pos,
					mk (TBlock [
						(* var field = fields[i++]; *)
						mk (TVar (field_var, Some (mk (TArray (local_fields, incr_i)) basic.tstring pos))) basic.tvoid pos;
						(
							(* default: Reflect.setField(new_me, field, Reflect.field(this, field)) *)
							let edef = gen.gtools.r_set_field basic.tvoid local_new_me local_field (gen.gtools.r_field false basic.tvoid this local_field) in
							if fields <> [] then
								(* switch(field) { ... } *)
								mk (TSwitch (local_field, fields_to_cases fields, Some edef)) basic.tvoid pos
							else
								edef;
						)
					]) basic.tvoid pos,
					NormalWhile
				)) basic.tvoid pos;
				(* return new_me *)
				mk_return local_new_me
			]) t_dynamic pos
		}
		in
		cfield.cf_expr <- Some (mk (TFunction fn) cfield.cf_type pos);
		cfield

	let create_static_cast_cf gen iface cf =
		let p = iface.cl_pos in
		let basic = gen.gcon.basic in
		let cparams = List.map (fun (s,t) -> ("To_" ^ s, TInst (map_param (get_cl_t t), []))) cf.cf_params in
		let me_type = TInst(iface,[]) in
		let cfield = mk_class_field ~static:true "__hx_cast" (TFun(["me",false,me_type], t_dynamic)) false iface.cl_pos (Method MethNormal) (cparams) in
		let params = List.map snd cparams in

		let me = alloc_var "me" me_type in
		let field = { eexpr = TField(mk_local me p, FInstance(iface, List.map snd iface.cl_params, cf)); etype = apply_params cf.cf_params params cf.cf_type; epos = p } in
		let call =
		{
			eexpr = TCall(field, []);
			etype = t_dynamic;
			epos = p;
		} in
		let call = gen.gparam_func_call call field params [] in

		(* since object.someCall<ExplicitParameterDefinition>() isn't allowed on Haxe, we need to directly apply the params and delay this call *)
		let delay () =
			cfield.cf_expr <-
			Some {
				eexpr = TFunction(
				{
					tf_args = [me,None];
					tf_type = t_dynamic;
					tf_expr = mk_return {
						eexpr = TIf(
							{ eexpr = TBinop(Ast.OpNotEq, mk_local me p, null me.v_type p); etype = basic.tbool; epos = p },
							call,
							Some( null me.v_type p )
						);
						etype = t_dynamic;
						epos = p;
					}
				});
				etype = cfield.cf_type;
				epos = p;
			}
		in
		cfield, delay

	let default_implementation gen ifaces base_generic =
		let add_iface cl =
			gen.gadd_to_module (TClassDecl cl) (max_dep);
		in

		let implement_stub_cast cthis iface tl =
			let name = get_cast_name iface in
			if not (PMap.mem name cthis.cl_fields) then begin
				let cparams = List.map (fun (s,t) -> ("To_" ^ s, TInst(map_param (get_cl_t t), []))) iface.cl_params in
				let field = mk_class_field name (TFun([],t_dynamic)) false iface.cl_pos (Method MethNormal) cparams in
				let this = { eexpr = TConst TThis; etype = TInst(cthis, List.map snd cthis.cl_params); epos = cthis.cl_pos } in
				field.cf_expr <- Some {
					etype = TFun([],t_dynamic);
					epos = this.epos;
					eexpr = TFunction {
						tf_type = t_dynamic;
						tf_args = [];
						tf_expr = mk_block (mk_return this)
					}
				};
				cthis.cl_ordered_fields <- field :: cthis.cl_ordered_fields;
				cthis.cl_fields <- PMap.add name field cthis.cl_fields
			end
		in

		let rec run md =
			match md with
				| TClassDecl ({ cl_params = [] } as cl) ->
					(* see if we're implementing any generic interface *)
					let rec check (iface,tl) =
						if tl <> [] && set_hxgeneric gen (TClassDecl iface) then
							(* implement cast stub *)
							implement_stub_cast cl iface tl;
						List.iter (fun (s,stl) -> check (s, List.map (apply_params iface.cl_params tl) stl)) iface.cl_implements;
					in
					List.iter (check) cl.cl_implements;
					md
				| TClassDecl ({ cl_params = hd :: tl } as cl) when set_hxgeneric gen md ->
					let iface = mk_class cl.cl_module cl.cl_path cl.cl_pos in
					iface.cl_array_access <- Option.map (apply_params (cl.cl_params) (List.map (fun _ -> t_dynamic) cl.cl_params)) cl.cl_array_access;
					iface.cl_extern <- cl.cl_extern;
					iface.cl_module <- cl.cl_module;
					iface.cl_private <- cl.cl_private;
					iface.cl_meta <-
						(Meta.HxGen, [], cl.cl_pos)
						::
						(Meta.Custom "generic_iface", [(EConst(Int(string_of_int(List.length cl.cl_params))), cl.cl_pos)], cl.cl_pos)
						::
						iface.cl_meta;
					Hashtbl.add ifaces cl.cl_path iface;

					iface.cl_implements <- (base_generic, []) :: iface.cl_implements;
					iface.cl_interface <- true;
					cl.cl_implements <- (iface, []) :: cl.cl_implements;

					let name = get_cast_name cl in
					let cast_cf = create_cast_cfield gen cl name in
					if not cl.cl_interface then create_stub_casts gen cl cast_cf;

					let rec loop c = match c.cl_super with
						| None -> ()
						| Some(sup,_) -> try
							let siface = Hashtbl.find ifaces sup.cl_path in
							iface.cl_implements <- (siface,[]) :: iface.cl_implements;
							()
						with | Not_found -> loop sup
					in
					loop cl;

					(if not cl.cl_interface then cl.cl_ordered_fields <- cast_cf :: cl.cl_ordered_fields);
					let iface_cf = mk_class_field name cast_cf.cf_type false cast_cf.cf_pos (Method MethNormal) cast_cf.cf_params in
					let cast_static_cf, delay = create_static_cast_cf gen iface iface_cf in

					cl.cl_ordered_statics <- cast_static_cf :: cl.cl_ordered_statics;
					cl.cl_statics <- PMap.add cast_static_cf.cf_name cast_static_cf cl.cl_statics;
					gen.gafter_filters_ended <- delay :: gen.gafter_filters_ended; (* do not let filters alter this expression content *)

					iface_cf.cf_type <- cast_cf.cf_type;
					iface.cl_fields <- PMap.add name iface_cf iface.cl_fields;
					let fields = List.filter (fun cf -> match cf.cf_kind with
						| Var _ | Method MethDynamic -> false
						| _ ->
							let is_override = has_class_field_flag cf CfOverride in
							let cf_type = if is_override && not (Meta.has Meta.Overload cf.cf_meta) then
								match find_first_declared_field gen cl cf.cf_name with
									| Some(_,_,declared_t,_,_,_,_) -> declared_t
									| _ -> Globals.die "" __LOC__
							else
								cf.cf_type
							in

							not (has_type_params cf_type)
						) cl.cl_ordered_fields
					in
					let fields = List.map (fun f -> mk_class_field f.cf_name f.cf_type (has_class_field_flag f CfPublic) f.cf_pos f.cf_kind f.cf_params) fields in
					let fields = iface_cf :: fields in
					iface.cl_ordered_fields <- fields;
					List.iter (fun f -> iface.cl_fields <- PMap.add f.cf_name f iface.cl_fields) fields;

					add_iface iface;
					md
				| TTypeDecl _ | TAbstractDecl _ -> md
				| TEnumDecl _ ->
					ignore (set_hxgeneric gen md);
					md
				| _ -> ignore (set_hxgeneric gen md); md
		in
		run

	let configure gen mapping_func =
		gen.gmodule_filters#add name (PCustom priority) mapping_func

end;;

(* create a common interface without type parameters and only a __Cast<> function *)
let default_implementation gen (dyn_tparam_cast:texpr->t->texpr) ifaces =
	let change_expr e cl iface params =
		let field = mk_static_field_access_infer cl "__hx_cast" e.epos params in
		let elist = [mk_cast (TInst(iface,[])) e] in
		let call = { eexpr = TCall(field, elist); etype = t_dynamic; epos = e.epos } in

		gen.gparam_func_call call field params elist
	in

	let rec run e =
		match e.eexpr with
				| TCast(cast_expr, _) ->
					(* see if casting to a native generic class *)
					let t = gen.greal_type e.etype in
					let unifies =
						let ctype = gen.greal_type cast_expr.etype in
						match follow ctype with
						| TInst(cl,_) -> (try
							unify ctype t;
							true
						with | Unify_error el ->
							false)
						| _ -> false
					in
					let unifies = unifies && not (PMap.mem "cs_safe_casts" gen.gcon.defines.Define.values) in
					(match follow t with
						| TInst(cl, p1 :: pl) when is_hxgeneric (TClassDecl cl) && not unifies && not (Meta.has Meta.Enum cl.cl_meta) ->
							let iface = Hashtbl.find ifaces cl.cl_path in
							mk_cast e.etype (change_expr (Type.map_expr run cast_expr) cl iface (p1 :: pl))
						| _ -> Type.map_expr run e
					)
				| _ -> Type.map_expr run e
	in
	run

let configure gen (dyn_tparam_cast:texpr->t->texpr) ifaces base_generic =
	gen.ghas_tparam_cast_handler <- true;
	let traverse = default_implementation gen dyn_tparam_cast ifaces in
	gen.gsyntax_filters#add name (PCustom priority) traverse;
	RealTypeParamsModf.configure gen (RealTypeParamsModf.default_implementation gen ifaces base_generic)
