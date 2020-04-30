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
open Ast
open Codegen
open Texpr.Builder
open Type
open Gencommon

let add_static c cf =
	c.cl_statics <- PMap.add cf.cf_name cf c.cl_statics;
	c.cl_ordered_statics <- cf :: c.cl_ordered_statics

let add_field c cf override =
	c.cl_fields <- PMap.add cf.cf_name cf c.cl_fields;
	c.cl_ordered_fields <- cf :: c.cl_ordered_fields;
	if override then c.cl_overrides <- cf :: c.cl_overrides

let add_meta com en cl_enum =
	Option.may (fun expr ->
		let cf_meta = mk_field "__meta__" expr.etype expr.epos expr.epos in
		cf_meta.cf_expr <- Some expr;
		add_static cl_enum cf_meta;
	) (Texpr.build_metadata com.basic (TEnumDecl en));

type enclasses = {
	base : tclass;
	ctors : (string, tclass) PMap.t;
}

module EnumToClass2Modf = struct
	let name = "enum_to_class2_mod"
	let priority = solve_deps name [DBefore ReflectionCFs.priority; DBefore RealTypeParams.RealTypeParamsModf.priority]

	let convert gen ec_tbl base_class en =
		let pos = en.e_pos in

		(* create the class *)
		let cl_enum = mk_class en.e_module en.e_path pos in
		cl_enum.cl_super <- Some (base_class,[]);
		cl_enum.cl_extern <- en.e_extern;
		cl_enum.cl_meta <- [(Meta.Enum,[],pos); (Meta.NativeGen,[],pos)] @ cl_enum.cl_meta;

		(* mark the enum that it's generated as a class *)
		en.e_meta <- (Meta.Class,[],pos) :: en.e_meta;

		(* add metadata *)
		add_meta gen.gcon en cl_enum;

		let basic = gen.gcon.basic in
		let mk_array_decl t el p = mk_nativearray_decl gen t el p in

		(* add constructs field (for reflection) *)
		if has_feature gen.gcon "Type.getEnumConstructs" then begin
			let e_constructs = mk_array_decl basic.tstring (List.map (fun s -> make_string gen.gcon.basic s pos) en.e_names) pos in
			let cf_constructs = mk_field "__hx_constructs" e_constructs.etype pos pos in
			cf_constructs.cf_kind <- Var { v_read = AccNormal; v_write = AccNever };
			cf_constructs.cf_meta <- (Meta.ReadOnly,[],pos) :: (Meta.Protected,[],pos) :: cf_constructs.cf_meta;
			cf_constructs.cf_expr <- Some e_constructs;
			add_static cl_enum cf_constructs
		end;

		(* add the class to the module *)
		gen.gadd_to_module (TClassDecl cl_enum) max_dep;

		let eparamsToString = mk_static_field_access_infer base_class "paramsToString" pos [] in
		let eparamsGetHashCode = mk_static_field_access_infer base_class "paramsGetHashCode" pos [] in

		let e_pack, e_name = en.e_path in
		let cl_enum_t = TInst (cl_enum, []) in
		let cf_getTag_t = tfun [] basic.tstring in
		let cf_getParams_ret = basic.tarray (mk_anon (ref Closed)) in
		let cf_getParams_t = tfun [] cf_getParams_ret in
		let static_ctors = ref [] in
		let ctors_map = ref PMap.empty in
		let add_ctor name index =
			let ef = PMap.find name en.e_constrs in
			let pos = ef.ef_pos in

			let cl_ctor = mk_class en.e_module (e_pack, e_name ^ "_" ^ name) pos in
			cl_ctor.cl_final <- true;
			cl_ctor.cl_super <- Some (cl_enum, []);
			cl_ctor.cl_meta <- [
				(Meta.Enum,[],pos);
				(Meta.NativeGen,[],pos);
			] @ cl_ctor.cl_meta;
			ctors_map := PMap.add name cl_ctor !ctors_map;

			gen.gadd_to_module (TClassDecl cl_ctor) max_dep;

			let esuper = mk (TConst TSuper) cl_enum_t pos in
			let etag = make_string gen.gcon.basic name pos in
			let efields = ref [] in
			(match follow ef.ef_type with
				| TFun(_, _) ->
					(* erase type params *)
					let ef_type =
						let t = apply_params en.e_params (List.map (fun _ -> t_dynamic) en.e_params) ef.ef_type in
						apply_params ef.ef_params (List.map (fun _ -> t_dynamic) ef.ef_params) t
					in
					let params, ret = get_fun ef_type in

					let cl_ctor_t = TInst (cl_ctor,[]) in
					let other_en_v = alloc_var "en" cl_ctor_t in
					let other_en_local = mk_local other_en_v pos in
					let enumeq = mk_static_field_access_infer (get_cl (get_type gen ([],"Type"))) "enumEq" pos [t_dynamic] in
					let refeq = mk_static_field_access_infer (get_cl (get_type gen (["System"],"Object"))) "ReferenceEquals" pos [] in

					let param_equal_checks = ref [] in
					let ctor_block = ref [] in
					let ctor_args = ref [] in
					let static_ctor_args = ref [] in
					let ethis = mk (TConst TThis) cl_ctor_t pos in
					List.iter (fun (n,_,t) ->
						(* create a field for enum argument *)
						let cf_param = mk_field n t pos pos in
						cf_param.cf_kind <- Var { v_read = AccNormal; v_write = AccNever };
						cf_param.cf_meta <- (Meta.ReadOnly,[],pos) :: cf_param.cf_meta;
						add_field cl_ctor cf_param false;

						(* add static constructor method argument *)
						static_ctor_args := (alloc_var n t, None) :: !static_ctor_args;

						(* generate argument field access *)
						let efield = mk (TField (ethis, FInstance (cl_ctor, [], cf_param))) t pos in
						efields := efield :: !efields;

						(* add constructor argument *)
						let ctor_arg_v = alloc_var n t in
						ctor_args := (ctor_arg_v, None) :: !ctor_args;

						(* generate assignment for the constructor *)
						let assign = binop OpAssign efield (mk_local ctor_arg_v pos) t pos in
						ctor_block := assign :: !ctor_block;

						(* generate an enumEq check for the Equals method (TODO: extract this) *)
						let eotherfield = mk (TField (other_en_local, FInstance (cl_ctor, [], cf_param))) t pos in
						let e_enumeq_check = mk (TCall (enumeq, [efield; eotherfield])) basic.tbool pos in
						let e_param_check =
							mk (TIf (mk (TUnop (Not, Prefix, e_enumeq_check)) basic.tbool pos,
							         mk_return (make_bool gen.gcon.basic false pos),
							         None)
							) basic.tvoid pos in
						param_equal_checks := e_param_check :: !param_equal_checks;
					) (List.rev params);

					ctor_block := (mk (TCall(esuper,[make_int gen.gcon.basic index pos])) basic.tvoid pos) :: !ctor_block;

					let cf_ctor_t = TFun (params, basic.tvoid) in
					let cf_ctor = mk_class_field "new" cf_ctor_t true pos (Method MethNormal) [] in
					cf_ctor.cf_expr <- Some {
						eexpr = TFunction {
							tf_args = !ctor_args;
							tf_type = basic.tvoid;
							tf_expr = mk (TBlock !ctor_block) basic.tvoid pos;
						};
						etype = cf_ctor_t;
						epos = pos;
					};
					cl_ctor.cl_constructor <- Some cf_ctor;

					let cf_toString_t = TFun ([],basic.tstring) in
					let cf_toString = mk_class_field "toString" cf_toString_t true pos (Method MethNormal) [] in

					let etoString_args = mk_array_decl t_dynamic !efields pos in
					cf_toString.cf_expr <- Some {
						eexpr = TFunction {
							tf_args = [];
							tf_type = basic.tstring;
							tf_expr = mk_block (mk_return (
								mk (TCall (eparamsToString, [etag; etoString_args])) basic.tstring pos
							));
						};
						etype = cf_toString_t;
						epos = pos;
					};
					add_field cl_ctor cf_toString true;

					let cf_static_ctor = mk_class_field name ef_type true pos (Method MethNormal) [] in
					cf_static_ctor.cf_expr <- Some {
						eexpr = TFunction {
							tf_args = !static_ctor_args;
							tf_type = ef_type;
							tf_expr = mk_block (mk_return {eexpr = TNew(cl_ctor,[], (List.map (fun (v,_) -> mk_local v pos) !static_ctor_args)); etype = ef_type; epos = pos});
						};
						etype = ef_type;
						epos = pos;
					};
					static_ctors := cf_static_ctor :: !static_ctors;

					(* add Equals field *)
					begin
						let other_v = alloc_var "other" t_dynamic in
						let eother_local = mk_local other_v pos in
						let eas = mk (TIdent "__as__") t_dynamic pos in
						let ecast = mk (TCall(eas,[eother_local])) cl_ctor_t pos in

						let equals_exprs = ref (List.rev [
							mk (TIf (
								mk (TCall(refeq,[ethis;eother_local])) basic.tbool pos,
								mk_return (make_bool gen.gcon.basic true pos),
								None
							)) basic.tvoid pos;
							mk (TVar(other_en_v, Some ecast)) basic.tvoid pos;
							mk (TIf(
								mk (TBinop(OpEq,other_en_local,make_null cl_ctor_t pos)) basic.tbool pos,
								mk_return (make_bool gen.gcon.basic false pos),
								None
							)) basic.tvoid pos;
						]) in
						equals_exprs := (List.rev !param_equal_checks) @ !equals_exprs;
						equals_exprs := mk_return (make_bool gen.gcon.basic true pos) :: !equals_exprs;

						let cf_Equals_t = TFun([("other",false,t_dynamic)],basic.tbool) in
						let cf_Equals = mk_class_field "Equals" cf_Equals_t true pos (Method MethNormal) [] in
						cf_Equals.cf_expr <- Some {
							eexpr = TFunction {
								tf_args = [(other_v,None)];
								tf_type = basic.tbool;
								tf_expr = mk (TBlock (List.rev !equals_exprs)) basic.tvoid pos;
							};
							etype = cf_Equals_t;
							epos = pos;
						};
						add_field cl_ctor cf_Equals true;
					end;

					(* add GetHashCode field *)
					begin
						let cf_GetHashCode_t = TFun([],basic.tint) in
						let cf_GetHashCode = mk_class_field "GetHashCode" cf_GetHashCode_t true pos (Method MethNormal) [] in
						cf_GetHashCode.cf_expr <- Some {
							eexpr = TFunction {
								tf_args = [];
								tf_type = basic.tint;
								tf_expr = mk_block (mk_return (
									mk (TCall(eparamsGetHashCode, [make_int gen.gcon.basic index pos;etoString_args])) basic.tint pos
								));
							};
							etype = cf_GetHashCode_t;
							epos = pos;
						};
						add_field cl_ctor cf_GetHashCode true;
					end

				| _ ->
					let cf_ctor_t = TFun([], basic.tvoid) in
					let cf_ctor = mk_class_field "new" cf_ctor_t true pos (Method MethNormal) [] in
					cf_ctor.cf_expr <- Some {
						eexpr = TFunction {
							tf_args = [];
							tf_type = basic.tvoid;
							tf_expr = mk (TBlock [mk (TCall(esuper,[make_int gen.gcon.basic index pos])) basic.tvoid pos]) basic.tvoid pos;
						};
						etype = cf_ctor_t;
						epos = pos;
					};
					cl_ctor.cl_constructor <- Some cf_ctor;

					let cf_static_inst = mk_class_field name cl_enum_t true pos (Var { v_read = AccNormal; v_write = AccNever }) [] in
					cf_static_inst.cf_meta <- [Meta.ReadOnly,[],pos];
					cf_static_inst.cf_expr <- Some {
						eexpr = TNew(cl_ctor, [], []);
						etype = cl_enum_t;
						epos = pos;
					};

					static_ctors := cf_static_inst :: !static_ctors;
			);

			let cf_getTag = mk_class_field "getTag" cf_getTag_t true pos (Method MethNormal) [] in
			cf_getTag.cf_expr <- Some {
				eexpr = TFunction {
					tf_args = [];
					tf_type = basic.tstring;
					tf_expr = mk_block (mk_return etag);
				};
				etype = cf_getTag_t;
				epos = pos;
			};
			add_field cl_ctor cf_getTag true;

			if !efields <> [] then begin
				let cf_getParams = mk_class_field "getParams" cf_getParams_t true pos (Method MethNormal) [] in
				cf_getParams.cf_expr <- Some {
					eexpr = TFunction {
						tf_args = [];
						tf_type = cf_getParams_ret;
						tf_expr = mk_block (mk_return (mk (TArrayDecl !efields) cf_getParams_ret pos));
					};
					etype = cf_getParams_t;
					epos = pos;
				};
				add_field cl_ctor cf_getParams true
			end
		in


		(* generate constructor subclasses and add static functions to create them *)
		let i = ref 0 in
		List.iter (fun name -> add_ctor name !i; incr i) en.e_names;

		List.iter (add_static cl_enum) !static_ctors;

		Hashtbl.add ec_tbl en.e_path {
			base = cl_enum;
			ctors = !ctors_map;
		};

		TEnumDecl en

	let configure gen t enum_base_class =
		let run md = match md with
			| TEnumDecl e when is_hxgen md ->
				convert gen t enum_base_class e
			| _ ->
				md
		in
		gen.gmodule_filters#add name (PCustom priority) run
end;;


module EnumToClass2Exprf = struct
	let init com ec_tbl mk_enum_index_call =
		let rec run e =
			let get_converted_enum_classes et =
				let en = match follow et with
					| TEnum (en,_) -> en
					| _ -> raise Not_found
				in
				Hashtbl.find ec_tbl en.e_path
			in
			let mk_converted_enum_index_access f =
				let cl = (get_converted_enum_classes f.etype).base in
				let e_enum = { f with etype = TInst (cl, []) } in
				field e_enum "_hx_index" com.basic.tint e.epos
			in
			match e.eexpr with
			| TEnumIndex f ->
				let f = run f in
				(try
					mk_converted_enum_index_access f
				with Not_found ->
					mk_enum_index_call f e.epos)
			| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ([], "Type") }, { cf_name = "enumIndex" })) } as left, [f]) ->
				let f = run f in
				(try
					mk_converted_enum_index_access f
				with Not_found ->
					{ e with eexpr = TCall(left, [f]) })
			| TEnumParameter(f, ef, i) ->
				let f = run f in
				(* check if en was converted to class *)
				(* if it was, switch on tag field and change cond type *)
				let classes = get_converted_enum_classes f.etype in
				let cl_enum = classes.base in
				let f = { f with etype = TInst(cl_enum, []) } in

				let cl_ctor = PMap.find ef.ef_name classes.ctors in
				let ecast = mk (TCall (mk (TIdent "__as__") t_dynamic f.epos, [f])) (TInst (cl_ctor, [])) f.epos in

				(match ef.ef_type with
				| TFun (params, _) ->
					let fname, _, _ = List.nth params i in
					field ecast fname e.etype e.epos
				| _ -> Globals.die "" __LOC__)
			| _ ->
				Type.map_expr run e
		in
		run

	let name = "enum_to_class2_exprf"
	let priority = solve_deps name []

	let configure gen ec_tbl mk_enum_index_call =
		let run = init gen.gcon ec_tbl mk_enum_index_call in
		gen.gexpr_filters#add name (PCustom priority) run
end;;

let configure gen enum_base_class mk_enum_index_call =
	let ec_tbl = Hashtbl.create 10 in
	EnumToClass2Modf.configure gen ec_tbl enum_base_class;
	EnumToClass2Exprf.configure gen ec_tbl mk_enum_index_call;
