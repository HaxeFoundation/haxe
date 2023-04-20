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
open Common
open Globals
open Ast
open Type
open Texpr.Builder
open Gencommon

(* ******************************************* *)
(* EnumToClass *)
(* ******************************************* *)
(*
	For languages that don't support parameterized enums and/or metadata in enums, we need to transform
	enums into normal classes. This is done at the first module pass by creating new classes with the same
	path inside the modules, and removing the actual enum module by setting it as en extern.

	* The target must create its own strategy to deal with reflection. As it is right now, we will have a base class
	which the class will extend, create @:$IsEnum metadata for the class, and create @:alias() metadatas for the fields,
	with their tag order (as a string) as their alias. If you are using ReflectionCFs, then you don't have to worry
	about that, as it's already generating all information needed by the haxe runtime.
	so they can be
*)
let name = "enum_to_class"
let priority = solve_deps name []

type t = {
	ec_tbl : (path, tclass) Hashtbl.t;
}

let new_t () = {
	ec_tbl = Hashtbl.create 10
}

(* ******************************************* *)
(* EnumToClassModf *)
(* ******************************************* *)
(*
	The actual Module Filter that will transform the enum into a class

	dependencies:
		Should run before ReflectionCFs, in order to enable proper reflection access.
		Should run before RealTypeParams.RealTypeParamsModf, since generic enums must be first converted to generic classes
		It needs that the target platform implements __array__() as a shortcut to declare haxe.ds.Vector
*)
module EnumToClassModf =
struct
	let name = "enum_to_class_mod"
	let priority = solve_deps name [DBefore ReflectionCFs.priority; DBefore RealTypeParams.RealTypeParamsModf.priority]

	let pmap_exists fn pmap = try PMap.iter (fun a b -> if fn a b then raise Exit) pmap; false with | Exit -> true

	let has_any_meta en =
		let has_meta meta = List.exists (fun (m,_,_) -> match m with Meta.Custom _ -> true | _ -> false) meta in
		has_meta en.e_meta || pmap_exists (fun _ ef -> has_meta ef.ef_meta) en.e_constrs

	let convert gen t base_class base_param_class en =
		let handle_type_params = false in (* TODO: look into this *)
		let basic = gen.gcon.basic in
		let pos = en.e_pos in

		(* create the class *)
		let cl = mk_class en.e_module en.e_path pos in
		Hashtbl.add t.ec_tbl en.e_path cl;

		(match Texpr.build_metadata gen.gcon.basic (TEnumDecl en) with
			| Some expr ->
				let cf = mk_class_field ~static:true "__meta__" expr.etype false expr.epos (Var { v_read = AccNormal; v_write = AccNormal }) [] in
				cf.cf_expr <- Some expr;
				cl.cl_statics <- PMap.add "__meta__" cf cl.cl_statics;
				cl.cl_ordered_statics <- cf :: cl.cl_ordered_statics
			| _ -> ()
		);

		let super, has_params = if Meta.has Meta.FlatEnum en.e_meta then base_class, false else base_param_class, true in

		cl.cl_super <- Some(super,[]);
		if en.e_extern then add_class_flag cl CExtern;
		en.e_meta <- (Meta.Class, [], pos) :: en.e_meta;
		cl.cl_module <- en.e_module;
		cl.cl_meta <- ( Meta.Enum, [], pos ) :: cl.cl_meta;

		(match gen.gcon.platform with
			| Cs when Common.defined gen.gcon Define.CoreApiSerialize ->
				cl.cl_meta <- ( Meta.Meta, [ (efield( (EConst (Ident "System"), null_pos ), "Serializable" ), null_pos) ], null_pos ) :: cl.cl_meta
			| _ -> ());
		let c_types =
			if handle_type_params then
				List.map (fun tp -> {tp with ttp_type=TInst (map_param (get_cl_t tp.ttp_type), [])}) en.e_params
			else
				[]
		in

		cl.cl_params <- c_types;

		let i = ref 0 in
		let cfs = List.map (fun name ->
			let ef = PMap.find name en.e_constrs in
			let pos = ef.ef_pos in
			let old_i = !i in
			incr i;

			let cf = match follow ef.ef_type with
				| TFun(params,ret) ->
					let dup_types =
						if handle_type_params then
							List.map (fun tp -> {tp with ttp_type = TInst (map_param (get_cl_t tp.ttp_type), [])}) en.e_params
						else
							[]
					in

					let ef_type =
						let fn, types = if handle_type_params then extract_param_type, dup_types else (fun _ -> t_dynamic), en.e_params in
						let t = apply_params en.e_params (List.map fn types) ef.ef_type in
						apply_params ef.ef_params (List.map fn ef.ef_params) t
					in

					let params, ret = get_fun ef_type in
					let cf_params = if handle_type_params then dup_types @ ef.ef_params else [] in

					let cf = mk_class_field name ef_type true pos (Method MethNormal) cf_params in
					cf.cf_meta <- [];

					let tf_args = List.map (fun (name,opt,t) ->  (alloc_var name t, if opt then Some (Texpr.Builder.make_null t null_pos) else None) ) params in
					let arr_decl = mk_nativearray_decl gen t_dynamic (List.map (fun (v,_) -> mk_local v pos) tf_args) pos in
					let expr = {
						eexpr = TFunction({
							tf_args = tf_args;
							tf_type = ret;
							tf_expr = mk_block ( mk_return { eexpr = TNew(cl,extract_param_types dup_types, [make_int gen.gcon.basic old_i pos; arr_decl] ); etype = TInst(cl, extract_param_types dup_types); epos = pos } );
						});
						etype = ef_type;
						epos = pos
					} in
					cf.cf_expr <- Some expr;
					cf
				| _ ->
					let actual_t = match follow ef.ef_type with
						| TEnum(e, p) -> TEnum(e, List.map (fun _ -> t_dynamic) p)
						| _ -> die "" __LOC__
					in
					let cf = mk_class_field name actual_t true pos (Var { v_read = AccNormal; v_write = AccNever }) [] in
					let args = if has_params then
						[make_int gen.gcon.basic old_i pos; null (gen.gclasses.nativearray t_dynamic) pos]
					else
						[make_int gen.gcon.basic old_i pos]
					in
					cf.cf_meta <- [Meta.ReadOnly,[],pos];
					cf.cf_expr <- Some {
						eexpr = TNew(cl, List.map (fun _ -> t_empty) cl.cl_params, args);
						etype = TInst(cl, List.map (fun _ -> t_empty) cl.cl_params);
						epos = pos;
					};
					cf
			in
			cl.cl_statics <- PMap.add cf.cf_name cf cl.cl_statics;
			cf
		) en.e_names in
		let constructs_cf = mk_class_field ~static:true "__hx_constructs" (gen.gclasses.nativearray basic.tstring) true pos (Var { v_read = AccNormal; v_write = AccNever }) [] in
		constructs_cf.cf_meta <- [Meta.ReadOnly,[],pos];
		constructs_cf.cf_expr <- Some (mk_nativearray_decl gen basic.tstring (List.map (fun s -> { eexpr = TConst(TString s); etype = basic.tstring; epos = pos }) en.e_names) pos);

		cl.cl_ordered_statics <- constructs_cf :: cfs @ cl.cl_ordered_statics ;
		cl.cl_statics <- PMap.add "__hx_constructs" constructs_cf cl.cl_statics;

		let getTag_cf_type = tfun [] basic.tstring in
		let getTag_cf = mk_class_field "getTag" getTag_cf_type true pos (Method MethNormal) [] in
		add_class_field_flag getTag_cf CfFinal;
		getTag_cf.cf_expr <- Some {
			eexpr = TFunction {
				tf_args = [];
				tf_type = basic.tstring;
				tf_expr = mk_return (
					let e_constructs = mk_static_field_access_infer cl "__hx_constructs" pos [] in
					let e_this = mk (TConst TThis) (TInst (cl,[])) pos in
					let e_index = mk_field_access gen e_this "index" pos in
					{
						eexpr = TArray(e_constructs,e_index);
						etype = basic.tstring;
						epos = pos;
					}
				)
			};
			etype = getTag_cf_type;
			epos = pos;
		};

		cl.cl_ordered_fields <- getTag_cf :: cl.cl_ordered_fields ;
		cl.cl_fields <- PMap.add "getTag" getTag_cf cl.cl_fields;
		add_class_field_flag getTag_cf CfOverride;
		cl.cl_meta <- (Meta.NativeGen,[],cl.cl_pos) :: cl.cl_meta;
		gen.gadd_to_module (TClassDecl cl) (max_dep);

		TEnumDecl en

	(*
		traverse
			gen - gen context
			convert_all : bool - should we convert all enums? If set, convert_if_has_meta will be ignored.
			convert_if_has_meta : bool - should we convert only if it has meta?
			enum_base_class : tclass - the enum base class.
			should_be_hxgen : bool - should the created enum be hxgen?
	*)
	let configure gen t convert_all convert_if_has_meta enum_base_class param_enum_class =
		let convert e = convert gen t enum_base_class param_enum_class e in
		let run md =
			match md with
			| TEnumDecl e when is_hxgen md ->
				if convert_all then
					convert e
				else if convert_if_has_meta && has_any_meta e then
					convert e
				else if not (Meta.has Meta.FlatEnum e.e_meta) then
					convert e
				else begin
					(* take off the :hxgen meta from it, if there's any *)
					e.e_meta <- List.filter (fun (n,_,_) -> not (n = Meta.HxGen)) e.e_meta;
					md
				end
			| _ ->
				md
		in
		gen.gmodule_filters#add name (PCustom priority) run
end;;

(* ******************************************* *)
(* EnumToClassExprf *)
(* ******************************************* *)
(*
	Enum to class Expression Filter

	dependencies:
		Should run before TArrayTransform, since it generates array access expressions
*)
module EnumToClassExprf =
struct
	let name = "enum_to_class_exprf"
	let priority = solve_deps name [DBefore TArrayTransform.priority]

	let configure gen t mk_enum_index_call =
		let rec run e =
			let get_converted_enum_type et =
				let en, eparams = match follow (gen.gfollow#run_f et) with
					| TEnum(en,p) -> en, p
					| _ -> raise Not_found
				in
				let cl = Hashtbl.find t.ec_tbl en.e_path in
				TInst(cl, eparams)
			in

			match e.eexpr with
			| TEnumIndex f ->
				let f = run f in
				(try
					mk_field_access gen {f with etype = get_converted_enum_type f.etype} "index" e.epos
				with Not_found ->
					mk_enum_index_call f e.epos)
			| TCall (({eexpr = TField(_, FStatic({cl_path=[],"Type"},{cf_name="enumIndex"}))} as left), [f]) ->
				let f = run f in
				(try
					mk_field_access gen {f with etype = get_converted_enum_type f.etype} "index" e.epos
				with Not_found ->
					{ e with eexpr = TCall(left, [f]) })
			| TEnumParameter(f, _,i) ->
				let f = run f in
				(* check if en was converted to class *)
				(* if it was, switch on tag field and change cond type *)
				let f = try
					{ f with etype = get_converted_enum_type f.etype }
				with Not_found ->
					f
				in
				let cond_array = { (mk_field_access gen f "params" f.epos) with etype = gen.gclasses.nativearray t_dynamic } in
				index gen.gcon.basic cond_array i e.etype e.epos
			| _ ->
				Type.map_expr run e
		in
		gen.gexpr_filters#add name (PCustom priority) run

end;;

let configure gen convert_all convert_if_has_meta enum_base_class param_enum_class mk_enum_index_call =
	let t = new_t () in
	EnumToClassModf.configure gen t convert_all convert_if_has_meta enum_base_class param_enum_class;
	EnumToClassExprf.configure gen t mk_enum_index_call
