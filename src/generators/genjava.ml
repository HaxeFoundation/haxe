(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation

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

open JData
open Unix
open Ast
open Common
open Type
open Gencommon
open Gencommon.SourceWriter
open Printf
open Option
open ExtString
module SS = Set.Make(String)

let is_boxed_type t = match follow t with
	| TInst ({ cl_path = (["java";"lang"], "Boolean") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Double") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Integer") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Byte") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Short") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Character") }, [])
	| TInst ({ cl_path = (["java";"lang"], "Float") }, []) -> true
	| TAbstract ({ a_path = (["java";"lang"], "Boolean") }, [])
	| TAbstract ({ a_path = (["java";"lang"], "Double") }, [])
	| TAbstract ({ a_path = (["java";"lang"], "Integer") }, [])
	| TAbstract ({ a_path = (["java";"lang"], "Byte") }, [])
	| TAbstract ({ a_path = (["java";"lang"], "Short") }, [])
	| TAbstract ({ a_path = (["java";"lang"], "Character") }, [])
	| TAbstract ({ a_path = (["java";"lang"], "Float") }, []) -> true
	| _ -> false

let unboxed_type gen t tbyte tshort tchar tfloat = match follow t with
	| TInst ({ cl_path = (["java";"lang"], "Boolean") }, []) -> gen.gcon.basic.tbool
	| TInst ({ cl_path = (["java";"lang"], "Double") }, []) -> gen.gcon.basic.tfloat
	| TInst ({ cl_path = (["java";"lang"], "Integer") }, []) -> gen.gcon.basic.tint
	| TInst ({ cl_path = (["java";"lang"], "Byte") }, []) -> tbyte
	| TInst ({ cl_path = (["java";"lang"], "Short") }, []) -> tshort
	| TInst ({ cl_path = (["java";"lang"], "Character") }, []) -> tchar
	| TInst ({ cl_path = (["java";"lang"], "Float") }, []) -> tfloat
	| TAbstract ({ a_path = (["java";"lang"], "Boolean") }, []) -> gen.gcon.basic.tbool
	| TAbstract ({ a_path = (["java";"lang"], "Double") }, []) -> gen.gcon.basic.tfloat
	| TAbstract ({ a_path = (["java";"lang"], "Integer") }, []) -> gen.gcon.basic.tint
	| TAbstract ({ a_path = (["java";"lang"], "Byte") }, []) -> tbyte
	| TAbstract ({ a_path = (["java";"lang"], "Short") }, []) -> tshort
	| TAbstract ({ a_path = (["java";"lang"], "Character") }, []) -> tchar
	| TAbstract ({ a_path = (["java";"lang"], "Float") }, []) -> tfloat
	| _ -> assert false

let rec t_has_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> true
	| TEnum(_, params)
	| TAbstract(_, params)
	| TInst(_, params) -> List.exists t_has_type_param params
	| TFun(f,ret) -> t_has_type_param ret || List.exists (fun (_,_,t) -> t_has_type_param t) f
	| _ -> false

let is_dynamic gen t =
	match follow (gen.greal_type t) with
		| TDynamic _ -> true
		| _ -> false

let is_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, _) -> true
	| _ -> false

let rec t_has_type_param_shallow last t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> true
	| TEnum(_, params)
	| TAbstract(_, params)
	| TInst(_, params) when not last -> List.exists (t_has_type_param_shallow true) params
	| TFun(f,ret) when not last -> t_has_type_param_shallow true ret	|| List.exists (fun (_,_,t) -> t_has_type_param_shallow true t) f
	| _ -> false

let rec replace_type_param t = match follow t with
	| TInst({ cl_kind = KTypeParameter _ }, []) -> t_dynamic
	| TEnum(e, params) -> TEnum(e, List.map replace_type_param params)
	| TAbstract(a, params) -> TAbstract(a, List.map replace_type_param params)
	| TInst(cl, params) -> TInst(cl, List.map replace_type_param params)
	| _ -> t

let is_java_basic_type t =
	match follow t with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TInst( { cl_path = (["haxe"], "Int64") }, [] )
		| TAbstract( { a_path = ([], "Single") }, [] )
		| TAbstract( { a_path = (["java"], ("Int8" | "Int16" | "Char16" | "Int64")) }, [] )
		| TAbstract( { a_path =	([], "Int") }, [] )
		| TAbstract( { a_path =	([], "Float") }, [] )
		| TAbstract( { a_path =	([], "Bool") }, [] ) ->
			true
		| _ -> false

let is_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| _ -> false

let like_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[])
		| TAbstract ({ a_path = (["java";"lang"],"Boolean") },[])
		| TInst ({ cl_path = (["java";"lang"],"Boolean") },[]) ->
			true
		| _ -> false

let is_int_float gen t =
	match follow (gen.greal_type t) with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TAbstract( { a_path =	([], "Int") }, [] )
		| TAbstract( { a_path =	([], "Float") }, [] ) ->
			true
		| (TAbstract _ as t) when like_float t && not (like_i64 t)-> true
		| _ -> false

let parse_explicit_iface =
	let regex = Str.regexp "\\." in
	let parse_explicit_iface str =
		let split = Str.split regex str in
		let rec get_iface split pack =
			match split with
				| clname :: fn_name :: [] -> fn_name, (List.rev pack, clname)
				| pack_piece :: tl -> get_iface tl (pack_piece :: pack)
				| _ -> assert false
		in
		get_iface split []
	in parse_explicit_iface

let is_string t =
	match follow t with
		| TInst( { cl_path = ([], "String") }, [] ) -> true
		| _ -> false

let is_cl t = match follow t with
	| TInst({ cl_path = ["java";"lang"],"Class" },_)
	| TAbstract({ a_path = [], ("Class"|"Enum") },_) -> true
	| TAnon(a) when is_some (anon_class t) -> true
	| _ -> false


(* ******************************************* *)
(* JavaSpecificESynf *)
(* ******************************************* *)
(*
	Some Java-specific syntax filters that must run before ExpressionUnwrap

	dependencies:
		It must run before ExprUnwrap, as it may not return valid Expr/Statement expressions
		It must run before ClassInstance, as it will detect expressions that need unchanged TTypeExpr
		It must run after CastDetect, as it changes casts
		It must run after TryCatchWrapper, to change Std.is() calls inside there
*)
module JavaSpecificESynf =
struct
	let name = "java_specific_e"
	let priority = solve_deps name [ DBefore ExpressionUnwrap.priority; DBefore ClassInstance.priority; DAfter CastDetect.priority; DAfter TryCatchWrapper.priority ]

	let get_cl_from_t t =
		match follow t with
			| TInst(cl,_) -> cl
			| _ -> assert false

	let configure gen runtime_cl =
		let basic = gen.gcon.basic in
		let float_cl = get_cl ( get_type gen (["java";"lang"], "Double")) in
		let i8_md  = ( get_type gen (["java";"lang"], "Byte")) in
		let i16_md	= ( get_type gen (["java";"lang"], "Short")) in
		let i64_md	= ( get_type gen (["java";"lang"], "Long")) in
		let c16_md	= ( get_type gen (["java";"lang"], "Character")) in
		let f_md	= ( get_type gen (["java";"lang"], "Float")) in
		let bool_md = get_type gen (["java";"lang"], "Boolean") in

		let is_var = alloc_var "__is__" t_dynamic in

		let rec run e =
			match e.eexpr with
				(* Math changes *)
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "NaN" }) ) ->
					mk_static_field_access_infer float_cl "NaN" e.epos []
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "NEGATIVE_INFINITY" }) ) ->
					mk_static_field_access_infer float_cl "NEGATIVE_INFINITY" e.epos []
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "POSITIVE_INFINITY" }) ) ->
					mk_static_field_access_infer float_cl "POSITIVE_INFINITY" e.epos []
				| TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "isNaN"}) ) ->
					mk_static_field_access_infer float_cl "isNaN" e.epos []
				| TCall( ({ eexpr = TField( (_ as ef), FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = ("ffloor" as f) }) ) } as fe), p)
				| TCall( ({ eexpr = TField( (_ as ef), FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = ("fceil" as f) }) ) } as fe), p) ->
						Type.map_expr run { e with eexpr = TCall({ fe with eexpr = TField(ef, FDynamic (String.sub f 1 (String.length f - 1)))	}, p) }
				| TCall( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "floor" }) ) }, _)
				| TCall( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "round" }) ) }, _)
				| TCall( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "ceil" }) ) }, _) ->
						mk_cast basic.tint (Type.map_expr run { e with etype = basic.tfloat })
				| TCall( ( { eexpr = TField( _, FStatic({ cl_path = (["java";"lang"], "Math") }, { cf_name = "isFinite" }) ) } as efield ), [v]) ->
					{ e with eexpr = TCall( mk_static_field_access_infer runtime_cl "isFinite" efield.epos [], [run v] ) }
				(* end of math changes *)

				(* Std.is() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "is" })) },
						[ obj; { eexpr = TTypeExpr(md) } ]
					) ->
					let mk_is is_basic obj md =
						let obj = if is_basic then mk_cast t_dynamic obj else obj in
						{ e with eexpr = TCall( { eexpr = TLocal is_var; etype = t_dynamic; epos = e.epos }, [
							run obj;
							{ eexpr = TTypeExpr md; etype = t_dynamic (* this is after all a syntax filter *); epos = e.epos }
						] ) }
					in
					(match follow_module follow md with
						| TAbstractDecl({ a_path = ([], "Float") }) ->
							{
								eexpr = TCall(
									mk_static_field_access_infer runtime_cl "isDouble" e.epos [],
									[ run obj ]
								);
								etype = basic.tbool;
								epos = e.epos
							}
						| TAbstractDecl{ a_path = ([], "Int") } ->
							{
								eexpr = TCall(
									mk_static_field_access_infer runtime_cl "isInt" e.epos [],
									[ run obj ]
								);
								etype = basic.tbool;
								epos = e.epos
							}
						| TAbstractDecl{ a_path = ([], "Bool") } ->
							mk_is true obj bool_md
						| TAbstractDecl{ a_path = ([], "Single") } ->
							mk_is true obj f_md
						| TAbstractDecl{ a_path = (["java"], "Int8") } ->
							mk_is true obj i8_md
						| TAbstractDecl{ a_path = (["java"], "Int16") } ->
							mk_is true obj i16_md
						| TAbstractDecl{ a_path = (["java"], "Char16") } ->
							mk_is true obj c16_md
						| TAbstractDecl{ a_path = (["java"], "Int64") } ->
							mk_is true obj i64_md
						| TClassDecl{ cl_path = (["haxe"], "Int64") } ->
							mk_is true obj i64_md
						| TAbstractDecl{ a_path = ([], "Dynamic") }
						| TClassDecl{ cl_path = ([], "Dynamic") } ->
							(match obj.eexpr with
								| TLocal _ | TConst _ -> { e with eexpr = TConst(TBool true) }
								| _ -> { e with eexpr = TBlock([run obj; { e with eexpr = TConst(TBool true) }]) }
							)
						| _ ->
							mk_is false obj md
					)
				(* end Std.is() *)
				| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
		gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;


(* ******************************************* *)
(* JavaSpecificSynf *)
(* ******************************************* *)
(*
	Some Java-specific syntax filters that can run after ExprUnwrap

	dependencies:
		Runs after ExprUnwarp
*)
module JavaSpecificSynf =
struct
	let name = "java_specific"
	let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority; DBefore IntDivisionSynf.priority ]

	let java_hash s =
		let high_surrogate c = (c lsr 10) + 0xD7C0 in
		let low_surrogate c = (c land 0x3FF) lor 0xDC00 in
		let h = ref Int32.zero in
		let thirtyone = Int32.of_int 31 in
		(try
			UTF8.validate s;
			UTF8.iter (fun c ->
				let c = (UChar.code c) in
				if c > 0xFFFF then
					(h := Int32.add (Int32.mul thirtyone !h)
						(Int32.of_int (high_surrogate c));
					h := Int32.add (Int32.mul thirtyone !h)
						(Int32.of_int (low_surrogate c)))
				else
					h := Int32.add (Int32.mul thirtyone !h)
						(Int32.of_int c)
				) s
		with UTF8.Malformed_code ->
			String.iter (fun c ->
				h := Int32.add (Int32.mul thirtyone !h)
					(Int32.of_int (Char.code c))) s
		);
		!h

	let rec is_final_return_expr is_switch e =
		let is_final_return_expr = is_final_return_expr is_switch in
		match e.eexpr with
			| TReturn _
			| TThrow _ -> true
			(* this is hack to not use 'break' on switch cases *)
			| TLocal { v_name = "__fallback__" } when is_switch -> true
			| TCall( { eexpr = TLocal { v_name = "__goto__" } }, _ ) -> true
			| TParenthesis p | TMeta (_,p) -> is_final_return_expr p
			| TBlock bl -> is_final_return_block is_switch bl
			| TSwitch (_, el_e_l, edef) ->
				List.for_all (fun (_,e) -> is_final_return_expr e) el_e_l && Option.map_default is_final_return_expr false edef
			| TIf (_,eif, Some eelse) ->
				is_final_return_expr eif && is_final_return_expr eelse
			| TFor (_,_,e) ->
				is_final_return_expr e
			| TWhile (_,e,_) ->
				is_final_return_expr e
			| TFunction tf ->
				is_final_return_expr tf.tf_expr
			| TTry (e, ve_l) ->
				is_final_return_expr e && List.for_all (fun (_,e) -> is_final_return_expr e) ve_l
			| _ -> false

	and is_final_return_block is_switch el =
		match el with
			| [] -> false
			| final :: [] -> is_final_return_expr is_switch final
			| hd :: tl -> is_final_return_block is_switch tl

	let is_null e = match e.eexpr with | TConst(TNull) -> true | _ -> false

	let rec is_equatable gen t =
		match follow t with
			| TInst(cl,_) ->
				if cl.cl_path = (["haxe";"lang"], "IEquatable") then
					true
				else
					List.exists (fun (cl,p) -> is_equatable gen (TInst(cl,p))) cl.cl_implements
						|| (match cl.cl_super with | Some(cl,p) -> is_equatable gen (TInst(cl,p)) | None -> false)
			| _ -> false

	(*
		Changing string switch
		will take an expression like
		switch(str)
		{
			case "a":
			case "b":
		}

		and modify it to:
		{
			var execute_def = true;
			switch(str.hashCode())
			{
				case (hashcode of a):
					if (str == "a")
					{
						execute_def = false;
						..code here
					} //else if (str == otherVariableWithSameHashCode) {
						...
					}
				...
			}
			if (execute_def)
			{
				..default code
			}
		}

		this might actually be slower in some cases than a if/else approach, but it scales well and as a bonus,
		hashCode in java are cached, so we only have the performance hit once to cache it.
	*)
	let change_string_switch gen eswitch e1 ecases edefault =
		let basic = gen.gcon.basic in
		let is_final_ret = is_final_return_expr false eswitch in

		let has_default = is_some edefault in
		let block = ref [] in
		let local = match e1.eexpr with
			| TLocal _ -> e1
			| _ ->
				let var = mk_temp gen "svar" e1.etype in
				let added = { e1 with eexpr = TVar(var, Some(e1)); etype = basic.tvoid } in
				let local = mk_local var e1.epos in
				block := added :: !block;
				local
		in
		let execute_def_var = mk_temp gen "executeDef" gen.gcon.basic.tbool in
		let execute_def = mk_local execute_def_var e1.epos in
		let execute_def_set = { eexpr = TBinop(Ast.OpAssign, execute_def, { eexpr = TConst(TBool false); etype = basic.tbool; epos = e1.epos }); etype = basic.tbool; epos = e1.epos } in

		let hash_cache = ref None in

		let local_hashcode = ref { local with
			eexpr = TCall({ local with
				eexpr = TField(local, FDynamic "hashCode");
				etype = TFun([], basic.tint);
			}, []);
			etype = basic.tint
		} in

		let get_hash_cache () =
			match !hash_cache with
				| Some c -> c
				| None ->
					let var = mk_temp gen "hash" basic.tint in
					let cond = !local_hashcode in
					block := { eexpr = TVar(var, Some cond); etype = basic.tvoid; epos = local.epos } :: !block;
					let local = mk_local var local.epos in
					local_hashcode := local;
					hash_cache := Some local;
					local
		in

		let has_case = ref false in
		(* first we need to reorder all cases so all collisions are close to each other *)

		let get_str e = match e.eexpr with | TConst(TString s) -> s | _ -> assert false in
		let has_conflict = ref false in

		let rec reorder_cases unordered ordered =
			match unordered with
				| [] -> ordered
				| (el, e) :: tl ->
					let current = Hashtbl.create 1 in
					List.iter (fun e ->
						let str = get_str e in
						let hash = java_hash str in
						Hashtbl.add current hash true
					) el;

					let rec extract_fields cases found_cases ret_cases =
						match cases with
							| [] -> found_cases, ret_cases
							| (el, e) :: tl ->
								if List.exists (fun e -> Hashtbl.mem current (java_hash (get_str e)) ) el then begin
									has_conflict := true;
									List.iter (fun e -> Hashtbl.add current (java_hash (get_str e)) true) el;
									extract_fields tl ( (el, e) :: found_cases ) ret_cases
								end else
									extract_fields tl found_cases ( (el, e) :: ret_cases )
					in
					let found, remaining = extract_fields tl [] [] in
					let ret = if found <> [] then
						let ret = List.sort (fun (e1,_) (e2,_) -> compare (List.length e2) (List.length e1) ) ( (el, e) :: found ) in
						let rec loop ret acc =
							match ret with
								| (el, e) :: ( (_,_) :: _ as tl ) -> loop tl ( (true, el, e) :: acc )
								| (el, e) :: [] -> ( (false, el, e) :: acc )
								| _ -> assert false
						in
						List.rev (loop ret [])
					else
						(false, el, e) :: []
					in

					reorder_cases remaining (ordered @ ret)
		in

		let already_in_cases = Hashtbl.create 0 in
		let change_case (has_fallback, el, e) =
			let conds, el = List.fold_left (fun (conds,el) e ->
				has_case := true;
				match e.eexpr with
					| TConst(TString s) ->
						let hashed = java_hash s in
						let equals_test = {
							eexpr = TCall({ e with eexpr = TField(local, FDynamic "equals"); etype = TFun(["obj",false,t_dynamic],basic.tbool) }, [ e ]);
							etype = basic.tbool;
							epos = e.epos
						} in

						let hashed_expr = { eexpr = TConst(TInt hashed); etype = basic.tint; epos = e.epos } in
						let hashed_exprs = if !has_conflict then begin
							if Hashtbl.mem already_in_cases hashed then
								el
							else begin
								Hashtbl.add already_in_cases hashed true;
								hashed_expr :: el
							end
						end else hashed_expr :: el in

						let conds = match conds with
							| None -> equals_test
							| Some c ->
								(*
									if there is more than one case, we should test first if hash equals to the one specified.
									This way we can save a heavier string compare
								*)
								let equals_test = mk_paren {
									eexpr = TBinop(Ast.OpBoolAnd, { eexpr = TBinop(Ast.OpEq, get_hash_cache(), hashed_expr); etype = basic.tbool; epos = e.epos }, equals_test);
									etype = basic.tbool;
									epos = e.epos;
								} in

								{ eexpr = TBinop(Ast.OpBoolOr, equals_test, c); etype = basic.tbool; epos = e1.epos }
						in

						Some conds, hashed_exprs
					| _ -> assert false
			) (None,[]) el in
			let e = if has_default then Type.concat execute_def_set e else e in
			let e = if !has_conflict then Type.concat e { e with eexpr = TBreak; etype = basic.tvoid } else e in
			let e = {
				eexpr = TIf(get conds, e, None);
				etype = basic.tvoid;
				epos = e.epos
			} in

			let e = if has_fallback then { e with eexpr = TBlock([ e; mk_local (alloc_var "__fallback__" t_dynamic) e.epos]) } else e in

			(el, e)
		in

		let switch = { eswitch with
			eexpr = TSwitch(!local_hashcode, List.map change_case (reorder_cases ecases []), None);
		} in
		(if !has_case then begin
			(if has_default then block := { e1 with eexpr = TVar(execute_def_var, Some({ e1 with eexpr = TConst(TBool true); etype = basic.tbool })); etype = basic.tvoid } :: !block);
			block := switch :: !block
		end);
		(match edefault with
			| None -> ()
			| Some edef when not !has_case ->
				block := edef :: !block
			| Some edef ->
				let eelse = if is_final_ret then Some { eexpr = TThrow { eexpr = TConst(TNull); etype = t_dynamic; epos = edef.epos }; etype = basic.tvoid; epos = edef.epos } else None in
				block := { edef with eexpr = TIf(execute_def, edef, eelse); etype = basic.tvoid } :: !block
		);
		{ eswitch with eexpr = TBlock(List.rev !block) }


	let get_cl_from_t t =
		match follow t with
		| TInst(cl,_) -> cl
		| _ -> assert false

	let configure gen runtime_cl =
		(if java_hash "Testing string hashCode implementation from haXe" <> (Int32.of_int 545883604) then assert false);
		let basic = gen.gcon.basic in
		(* let tchar = mt_to_t_dyn ( get_type gen (["java"], "Char16") ) in *)
		(* let tbyte = mt_to_t_dyn ( get_type gen (["java"], "Int8") ) in *)
		(* let tshort = mt_to_t_dyn ( get_type gen (["java"], "Int16") ) in *)
		(* let tsingle = mt_to_t_dyn ( get_type gen ([], "Single") ) in *)
		let ti64 = mt_to_t_dyn ( get_type gen (["java"], "Int64") ) in
		let string_ext = get_cl ( get_type gen (["haxe";"lang"], "StringExt")) in
		let fast_cast = Common.defined gen.gcon Define.FastCast in

		let is_string t = match follow t with | TInst({ cl_path = ([], "String") }, []) -> true | _ -> false in

		let rec run e =
			match e.eexpr with
				(* for new NativeArray<T> issues *)
				| TNew(({ cl_path = (["java"], "NativeArray") } as cl), [t], el) when is_type_param t ->
					mk_cast (TInst(cl,[t])) (mk_cast t_dynamic ({ e with eexpr = TNew(cl, [t_empty], List.map run el) }))

				(* Std.int() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" })) },
						[obj]
					) ->
					run (mk_cast basic.tint obj)
				(* end Std.int() *)

				| TField( ef, FInstance({ cl_path = ([], "String") }, _, { cf_name = "length" }) ) ->
					{ e with eexpr = TCall(Type.map_expr run e, []) }
				| TField( ef, field ) when field_name field = "length" && is_string ef.etype ->
					{ e with eexpr = TCall(Type.map_expr run e, []) }
				| TCall( ( { eexpr = TField(ef, field) } as efield ), args ) when is_string ef.etype && String.get (field_name field) 0 = '_' ->
					let field = field_name field in
					{ e with eexpr = TCall({ efield with eexpr = TField(run ef, FDynamic (String.sub field 1 ( (String.length field) - 1)) )}, List.map run args) }
				| TCall( ( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, field )) } as efield ), args ) ->
					let field = field.cf_name in
					(match field with
						| "charAt" | "charCodeAt" | "split" | "indexOf"
						| "lastIndexOf" | "substring" | "substr" ->
							{ e with eexpr = TCall(mk_static_field_access_infer string_ext field e.epos [], [run ef] @ (List.map run args)) }
						| _ ->
							{ e with eexpr = TCall(run efield, List.map run args) }
					)
(*				 | TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, { cf_name = ("toString") })) }, [] ) ->
					run ef *)

				(* | TCast(expr, m) when is_boxed_type e.etype -> *)
				(* 	(* let unboxed_type gen t tbyte tshort tchar tfloat = match follow t with *) *)
				(* 	run { e with etype = unboxed_type gen e.etype tbyte tshort tchar tsingle } *)

				| TCast(expr, _) when is_bool e.etype && is_dynamic gen expr.etype ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toBool" expr.epos [],
							[ run expr ]
						);
						etype = basic.tbool;
						epos = e.epos
					}

				| TCast(expr, _) when is_int_float gen e.etype && is_dynamic gen expr.etype ->
					let needs_cast = match gen.gfollow#run_f e.etype with
						| TInst _ -> false
						| _ -> true
					in

					let fun_name = if like_int e.etype then "toInt" else "toDouble" in

					let ret = {
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl fun_name expr.epos [],
							[ run expr ]
						);
						etype = if fun_name = "toDouble" then basic.tfloat else basic.tint;
						epos = expr.epos
					} in

					if needs_cast then mk_cast e.etype ret else ret

				(*| TCast(expr, c) when is_int_float gen e.etype ->
					(* cases when float x = (float) (java.lang.Double val); *)
					(* FIXME: this fix is broken since it will fail on cases where float x = (float) (java.lang.Float val) or similar. FIX THIS *)
					let need_second_cast = match gen.gfollow#run_f e.etype with
						| TInst _ -> false
						| _ -> true
					in
					if need_second_cast then { e with eexpr = TCast(mk_cast (follow e.etype) (run expr), c) }  else Type.map_expr run e*)
				| TCast(expr, _) when like_i64 e.etype && is_dynamic gen expr.etype ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toLong" expr.epos [],
							[ run expr ]
						);
						etype = ti64;
						epos = expr.epos
					}

				| TCast(expr, Some(TClassDecl cls)) when fast_cast && cls == null_class ->
					{ e with eexpr = TCast(run expr, Some(TClassDecl null_class)) }

				| TBinop( (Ast.OpAssignOp OpAdd as op), e1, e2)
				| TBinop( (Ast.OpAdd as op), e1, e2) when not fast_cast && (is_string e.etype || is_string e1.etype || is_string e2.etype) ->
						let is_assign = match op with Ast.OpAssignOp _ -> true | _ -> false in
						let mk_to_string e = { e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" e.epos [], [run e] ); etype = gen.gcon.basic.tstring	} in
						let check_cast e = match gen.greal_type e.etype with
							| TDynamic _
							| TAbstract({ a_path = ([], "Float") }, [])
							| TAbstract({ a_path = ([], "Single") }, []) ->
									mk_to_string e
							| _ -> run e
						in

						{ e with eexpr = TBinop(op, (if is_assign then run e1 else check_cast e1), check_cast e2) }
				| TCast(expr, _) when is_string e.etype ->
					{ e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" expr.epos [], [run expr] ) }

				| TSwitch(cond, ecases, edefault) when is_string cond.etype ->
					(*let change_string_switch gen eswitch e1 ecases edefault =*)
					change_string_switch gen e (run cond) (List.map (fun (el,e) -> (el, run e)) ecases) (Option.map run edefault)

				| TBinop( (Ast.OpNotEq as op), e1, e2)
				| TBinop( (Ast.OpEq as op), e1, e2) when not (is_null e2 || is_null e1) && (is_string e1.etype || is_string e2.etype || is_equatable gen e1.etype || is_equatable gen e2.etype) ->
					let static = mk_static_field_access_infer (runtime_cl) "valEq" e1.epos [] in
					let eret = { eexpr = TCall(static, [run e1; run e2]); etype = gen.gcon.basic.tbool; epos=e.epos } in
					if op = Ast.OpNotEq then { eret with eexpr = TUnop(Ast.Not, Ast.Prefix, eret) } else eret

				| TBinop( (Ast.OpNotEq | Ast.OpEq as op), e1, e2) when is_cl e1.etype && is_cl e2.etype ->
					{ e with eexpr = TBinop(op, mk_cast t_empty (run e1), mk_cast t_empty (run e2)) }
				| _ -> Type.map_expr run e
		in
		let map e = Some(run e) in
		gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map

end;;


(* ******************************************* *)
(* handle @:throws *)
(* ******************************************* *)
let rec is_checked_exc cl =
	match cl.cl_path with
		| ["java";"lang"],"RuntimeException" ->
			false
		| ["java";"lang"],"Throwable" ->
			true
		| _ -> match cl.cl_super with
			| None -> false
			| Some(c,_) -> is_checked_exc c

let rec cls_any_super cl supers =
	PMap.mem cl.cl_path supers || match cl.cl_super with
		| None -> false
		| Some(c,_) -> cls_any_super c supers

let rec handle_throws gen cf =
	List.iter (handle_throws gen) cf.cf_overloads;
	match cf.cf_expr with
	| Some ({ eexpr = TFunction(tf) } as e)  ->
		let rec collect_throws acc = function
			| (Meta.Throws, [Ast.EConst (Ast.String path), _],_) :: meta -> (try
				collect_throws (get_cl ( get_type gen (parse_path path)) :: acc) meta
			with | Not_found | TypeNotFound _ ->
				collect_throws acc meta)
			| [] ->
				acc
			| _ :: meta ->
				collect_throws acc meta
		in
		let cf_throws = collect_throws [] cf.cf_meta in
		let throws = ref (List.fold_left (fun map cl ->
			PMap.add cl.cl_path cl map
		) PMap.empty cf_throws) in
		let rec iter e = match e.eexpr with
			| TTry(etry,ecatches) ->
				let old = !throws in
				let needs_check_block = ref true in
				List.iter (fun (v,e) ->
					Type.iter iter e;
					match follow (run_follow gen v.v_type) with
						| TInst({ cl_path = ["java";"lang"],"Throwable" },_)
						| TDynamic _ ->
							needs_check_block := false
						| TInst(c,_) when is_checked_exc c ->
							throws := PMap.add c.cl_path c !throws
						| _ ->()
				) ecatches;
				if !needs_check_block then Type.iter iter etry;
				throws := old
			| TField(e, (FInstance(_,_,f) | FStatic(_,f) | FClosure(_,f))) ->
				let tdefs = collect_throws [] f.cf_meta in
				if tdefs <> [] && not (List.for_all (fun c -> cls_any_super c !throws) tdefs) then
					raise Exit;
				Type.iter iter e
			| TThrow e -> (match follow (run_follow gen e.etype) with
				| TInst(c,_) when is_checked_exc c && not (cls_any_super c !throws) ->
					raise Exit
				| _ -> iter e)
			| _ -> Type.iter iter e
		in
		(try
			Type.iter iter e
		with | Exit -> (* needs typed exception to be caught *)
			let throwable = get_cl (get_type gen (["java";"lang"],"Throwable")) in
			let catch_var = alloc_var "typedException" (TInst(throwable,[])) in
			let rethrow = mk_local catch_var e.epos in
			let hx_exception = get_cl (get_type gen (["haxe";"lang"], "HaxeException")) in
			let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], t_dynamic)) rethrow.epos in
			let wrapped = { rethrow with eexpr = TThrow { rethrow with eexpr = TCall(wrap_static, [rethrow]) }; } in
			let map_throws cl =
				let var = alloc_var "typedException" (TInst(cl,List.map (fun _ -> t_dynamic) cl.cl_params)) in
				var, { tf.tf_expr with eexpr = TThrow (mk_local var e.epos) }
			in
			cf.cf_expr <- Some { e with
				eexpr = TFunction({ tf with
					tf_expr = mk_block { tf.tf_expr with eexpr = TTry(tf.tf_expr, List.map (map_throws) cf_throws @ [catch_var, wrapped]) }
				})
			})
	| _ -> ()


let connecting_string = "?" (* ? see list here http://www.fileformat.info/info/unicode/category/index.htm and here for C# http://msdn.microsoft.com/en-us/library/aa664670.aspx *)
let default_package = "java"
let strict_mode = ref false (* strict mode is so we can check for unexpected information *)

(* reserved java words *)
let reserved = let res = Hashtbl.create 120 in
	List.iter (fun lst -> Hashtbl.add res lst ("_" ^ lst)) ["abstract"; "assert"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class";
		"const"; "continue"; "default"; "do"; "double"; "else"; "enum"; "extends"; "final";
		"false"; "finally"; "float"; "for"; "goto"; "if"; "implements"; "import"; "instanceof"; "int";
		"interface"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "short";
		"static"; "strictfp"; "super"; "switch"; "synchronized"; "this"; "throw"; "throws"; "transient"; "true"; "try";
		"void"; "volatile"; "while"; ];
	res

let dynamic_anon = TAnon( { a_fields = PMap.empty; a_status = ref Closed } )

let rec get_class_modifiers meta cl_type cl_access cl_modifiers =
	match meta with
		| [] -> cl_type,cl_access,cl_modifiers
		(*| (Meta.Struct,[],_) :: meta -> get_class_modifiers meta "struct" cl_access cl_modifiers*)
		| (Meta.Protected,[],_) :: meta -> get_class_modifiers meta cl_type "protected" cl_modifiers
		| (Meta.Internal,[],_) :: meta -> get_class_modifiers meta cl_type "" cl_modifiers
		(* no abstract for now | (":abstract",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("abstract" :: cl_modifiers)
		| (Meta.Static,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("static" :: cl_modifiers) TODO: support those types *)
		| (Meta.Final,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("final" :: cl_modifiers)
		| _ :: meta -> get_class_modifiers meta cl_type cl_access cl_modifiers

let rec get_fun_modifiers meta access modifiers =
	match meta with
		| [] -> access,modifiers
		| (Meta.Protected,[],_) :: meta -> get_fun_modifiers meta "protected" modifiers
		| (Meta.Internal,[],_) :: meta -> get_fun_modifiers meta "" modifiers
		| (Meta.ReadOnly,[],_) :: meta -> get_fun_modifiers meta access ("final" :: modifiers)
		(*| (Meta.Unsafe,[],_) :: meta -> get_fun_modifiers meta access ("unsafe" :: modifiers)*)
		| (Meta.Volatile,[],_) :: meta -> get_fun_modifiers meta access ("volatile" :: modifiers)
		| (Meta.Transient,[],_) :: meta -> get_fun_modifiers meta access ("transient" :: modifiers)
		| (Meta.Native,[],_) :: meta -> get_fun_modifiers meta access ("native" :: modifiers)
		| _ :: meta -> get_fun_modifiers meta access modifiers

(* this was the way I found to pass the generator context to be accessible across all functions here *)
(* so 'configure' is almost 'top-level' and will have all functions needed to make this work *)
let configure gen =
	let native_arr_cl = get_cl ( get_type gen (["java"], "NativeArray") ) in
	gen.gclasses.nativearray <- (fun t -> TInst(native_arr_cl,[t]));
	gen.gclasses.nativearray_type <- (function TInst(_,[t]) -> t | _ -> assert false);
	gen.gclasses.nativearray_len <- (fun e p -> mk_field_access gen e "length" p);

	let basic = gen.gcon.basic in

	let fn_cl = get_cl (get_type gen (["haxe";"lang"],"Function")) in

	let runtime_cl = get_cl (get_type gen (["haxe";"lang"],"Runtime")) in
	let nulltdef = get_tdef (get_type gen ([],"Null")) in

	(*let string_ref = get_cl ( get_type gen (["haxe";"lang"], "StringRefl")) in*)

	let ti64 = match ( get_type gen (["java"], "Int64") ) with | TAbstractDecl a -> TAbstract(a,[]) | _ -> assert false in

	let has_tdynamic params =
		List.exists (fun e -> match run_follow gen e with | TDynamic _ -> true | _ -> false) params
	in

	(*
		The type parameters always need to be changed to their boxed counterparts
	*)
	let change_param_type md params =
		match md with
			| TClassDecl( { cl_path = (["java"], "NativeArray") } ) -> params
			| TAbstractDecl { a_path=[],("Class" | "Enum") } | TClassDecl { cl_path = (["java";"lang"],("Class"|"Enum")) } ->
				List.map (fun _ -> t_dynamic) params
			| _ ->
				match params with
					| [] -> []
					| _ ->
						if has_tdynamic params then List.map (fun _ -> t_dynamic) params else
							List.map (fun t ->
								let f_t = gen.gfollow#run_f t in
								match f_t  with
									| TAbstract ({ a_path = ([], "Bool") },[])
									| TAbstract ({ a_path = ([],"Float") },[])
									| TInst ({ cl_path = ["haxe"],"Int32" },[])
									| TInst ({ cl_path = ["haxe"],"Int64" },[])
									| TAbstract ({ a_path = ([],"Int") },[])
									| TType ({ t_path = ["java"], "Int64" },[])
									| TAbstract ({ a_path = ["java"], "Int64" },[])
									| TType ({ t_path = ["java"],"Int8" },[])
									| TAbstract ({ a_path = ["java"],"Int8" },[])
									| TType ({ t_path = ["java"],"Int16" },[])
									| TAbstract ({ a_path = ["java"],"Int16" },[])
									| TType ({ t_path = ["java"],"Char16" },[])
									| TAbstract ({ a_path = ["java"],"Char16" },[])
									| TType ({ t_path = [],"Single" },[])
									| TAbstract ({ a_path = [],"Single" },[]) ->
										TType(nulltdef, [f_t])
									(*| TType ({ t_path = [], "Null"*)
									| TInst (cl, ((_ :: _) as p)) when cl.cl_path <> (["java"],"NativeArray") ->
										(* TInst(cl, List.map (fun _ -> t_dynamic) p) *)
										TInst(cl,p)
									| TEnum (e, ((_ :: _) as p)) ->
										TEnum(e, List.map (fun _ -> t_dynamic) p)
									| _ -> t
							) params
	in

	let change_clname name =
		String.map (function | '$' -> '.' | c -> c) name
	in
	let change_id name = try Hashtbl.find reserved name with | Not_found -> name in
	let rec change_ns ns = match ns with
		| [] -> ["haxe"; "root"]
		| _ -> List.map change_id ns
	in
	let change_field = change_id in

	let write_id w name = write w (change_id name) in

	let write_field w name = write w (change_field name) in

	gen.gfollow#add ~name:"follow_basic" (fun t -> match t with
			| TAbstract ({ a_path = ([], "Bool") },[])
			| TAbstract ({ a_path = ([], "Void") },[])
			| TAbstract ({ a_path = ([],"Float") },[])
			| TAbstract ({ a_path = ([],"Int") },[])
			| TInst( { cl_path = (["haxe"], "Int32") }, [] )
			| TInst( { cl_path = (["haxe"], "Int64") }, [] )
			| TType ({ t_path = ["java"], "Int64" },[])
			| TAbstract ({ a_path = ["java"], "Int64" },[])
			| TType ({ t_path = ["java"],"Int8" },[])
			| TAbstract ({ a_path = ["java"],"Int8" },[])
			| TType ({ t_path = ["java"],"Int16" },[])
			| TAbstract ({ a_path = ["java"],"Int16" },[])
			| TType ({ t_path = ["java"],"Char16" },[])
			| TAbstract ({ a_path = ["java"],"Char16" },[])
			| TType ({ t_path = [],"Single" },[])
			| TAbstract ({ a_path = [],"Single" },[]) ->
					Some t
			| TType (({ t_path = [],"Null" } as tdef),[t2]) ->
					Some (TType(tdef,[gen.gfollow#run_f t2]))
			| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					Some (gen.gfollow#run_f ( Abstract.get_underlying_type a pl) )
			| TAbstract( { a_path = ([], "EnumValue") }, _ )
			| TInst( { cl_path = ([], "EnumValue") }, _  ) -> Some t_dynamic
			| _ -> None);

	let change_path path = (change_ns (fst path), change_clname (snd path)) in

	let path_s path meta = try
		match Meta.get Meta.JavaCanonical meta with
			| (Meta.JavaCanonical, [EConst(String pack), _; EConst(String name), _], _) ->
				if pack = "" then
					name
				else
					pack ^ "." ^ name
			| _ -> raise Not_found
		with Not_found -> match path with
			| (ns,clname) -> s_type_path (change_ns ns, change_clname clname)
	in

	let cl_cl = get_cl (get_type gen (["java";"lang"],"Class")) in

	let rec real_type t =
		let t = gen.gfollow#run_f t in
		match t with
			| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
				real_type (Abstract.get_underlying_type a pl)
			| TInst( { cl_path = (["haxe"], "Int32") }, [] ) -> gen.gcon.basic.tint
			| TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> ti64
			| TAbstract( { a_path = ([], "Class") }, p	)
			| TAbstract( { a_path = ([], "Enum") }, p  )
			| TInst( { cl_path = ([], "Class") }, p  )
			| TInst( { cl_path = ([], "Enum") }, p	) -> TInst(cl_cl,[t_dynamic])
			| TEnum(e,params) -> TEnum(e, List.map (fun _ -> t_dynamic) params)
			| TInst(c,params) when Meta.has Meta.Enum c.cl_meta ->
				TInst(c, List.map (fun _ -> t_dynamic) params)
			| TInst({ cl_kind = KExpr _ }, _) -> t_dynamic
			| TInst _ -> t
			| TType({ t_path = ([], "Null") }, [t]) when is_java_basic_type (gen.gfollow#run_f t) -> t_dynamic
			| TType({ t_path = ([], "Null") }, [t]) ->
				(match follow t with
					| TInst( { cl_kind = KTypeParameter _ }, []) ->
							t_dynamic
							(* real_type t *)
					| _ -> real_type t
				)
			| TType _ | TAbstract _ -> t
			| TAnon (anon) -> (match !(anon.a_status) with
				| Statics _ | EnumStatics _ | AbstractStatics _ -> t
				| _ -> t_dynamic)
			| TFun _ -> TInst(fn_cl,[])
			| _ -> t_dynamic
	in

	let scope = ref PMap.empty in
	let imports = ref [] in

	let clear_scope () =
		scope := PMap.empty;
		imports := [];
	in

	let add_scope name =
		scope := PMap.add name () !scope
	in

	let add_import pos path meta =
		let name = snd path in
		let rec loop = function
			| (pack, n) :: _ when name = n ->
					if path <> (pack,n) then
						gen.gcon.error ("This expression cannot be generated because " ^ path_s path meta ^ " is shadowed by the current scope and ") pos
			| _ :: tl ->
					loop tl
			| [] ->
					(* add import *)
					imports := path :: !imports
		in
		loop !imports
	in

	let path_s_import pos path meta = match path with
		| [], name when PMap.mem name !scope ->
				gen.gcon.error ("This expression cannot be generated because " ^ name ^ " is shadowed by the current scope") pos;
				name
		| pack1 :: _, name when PMap.mem pack1 !scope -> (* exists in scope *)
				add_import pos path meta;
				(* check if name exists in scope *)
				if PMap.mem name !scope then
					gen.gcon.error ("This expression cannot be generated because " ^ pack1 ^ " and " ^ name ^ " are both shadowed by the current scope") pos;
				name
		| _ -> path_s path meta
	in

	let is_dynamic t = match real_type t with
		| TMono _ | TDynamic _
		| TInst({ cl_kind = KTypeParameter _ }, _) -> true
		| TAnon anon ->
			(match !(anon.a_status) with
				| EnumStatics _ | Statics _ | AbstractStatics _ -> false
				| _ -> true
			)
		| _ -> false
	in

	let rec t_s pos t =
		match real_type t with
			(* basic types *)
			| TAbstract ({ a_path = ([], "Bool") },[]) -> "boolean"
			| TAbstract ({ a_path = ([], "Void") },[]) ->
					path_s_import pos (["java";"lang"], "Object") []
			| TAbstract ({ a_path = ([],"Float") },[]) -> "double"
			| TAbstract ({ a_path = ([],"Int") },[]) -> "int"
			| TType ({ t_path = ["java"], "Int64" },[])
			| TAbstract ({ a_path = ["java"], "Int64" },[]) -> "long"
			| TType ({ t_path = ["java"],"Int8" },[])
			| TAbstract ({ a_path = ["java"],"Int8" },[]) -> "byte"
			| TType ({ t_path = ["java"],"Int16" },[])
			| TAbstract ({ a_path = ["java"],"Int16" },[]) -> "short"
			| TType ({ t_path = ["java"],"Char16" },[])
			| TAbstract ({ a_path = ["java"],"Char16" },[]) -> "char"
			| TType ({ t_path = [],"Single" },[])
			| TAbstract ({ a_path = [],"Single" },[]) -> "float"
			| TInst ({ cl_path = ["haxe"],"Int32" },[])
			| TAbstract ({ a_path = ["haxe"],"Int32" },[]) -> "int"
			| TInst ({ cl_path = ["haxe"],"Int64" },[])
			| TAbstract ({ a_path = ["haxe"],"Int64" },[]) -> "long"
			| TInst({ cl_path = (["java"], "NativeArray") }, [param]) ->
				let rec check_t_s t =
					match real_type t with
						| TInst({ cl_path = (["java"], "NativeArray") }, [param]) ->
							(check_t_s param) ^ "[]"
						| _ -> t_s pos (run_follow gen t)
				in
				(check_t_s param) ^ "[]"

			(* end of basic types *)
			| TInst ({ cl_kind = KTypeParameter _; cl_path=p }, []) -> snd p
			| TAbstract ({ a_path = [], "Dynamic" },[]) ->
					path_s_import pos (["java";"lang"], "Object") []
			| TMono r -> (match !r with | None -> "java.lang.Object" | Some t -> t_s pos (run_follow gen t))
			| TInst ({ cl_path = [], "String" }, []) ->
					path_s_import pos (["java";"lang"], "String") []
			| TAbstract ({ a_path = [], "Class" }, [p]) | TAbstract ({ a_path = [], "Enum" }, [p])
			| TInst ({ cl_path = [], "Class" }, [p]) | TInst ({ cl_path = [], "Enum" }, [p]) ->
					path_param_s pos (TClassDecl cl_cl) (["java";"lang"], "Class") [p] []
			| TAbstract ({ a_path = [], "Class" }, _) | TAbstract ({ a_path = [], "Enum" }, _)
			| TInst ({ cl_path = [], "Class" }, _) | TInst ({ cl_path = [], "Enum" }, _) ->
					path_s_import pos (["java";"lang"], "Class") []
			| TEnum ({e_path = p; e_meta = meta}, _) ->
					path_s_import pos p meta
			| TInst (({cl_path = p; cl_meta = meta} as cl), _) when Meta.has Meta.Enum cl.cl_meta ->
					path_s_import pos p meta
			| TInst (({cl_path = p; cl_meta = meta} as cl), params) -> (path_param_s pos (TClassDecl cl) p params meta)
			| TType (({t_path = p; t_meta = meta} as t), params) -> (path_param_s pos (TTypeDecl t) p params meta)
			| TAnon (anon) ->
				(match !(anon.a_status) with
					| Statics _ | EnumStatics _ | AbstractStatics _ ->
							path_s_import pos (["java";"lang"], "Class") []
					| _ ->
							path_s_import pos (["java";"lang"], "Object") [])
				| TDynamic _ ->
						path_s_import pos (["java";"lang"], "Object") []
			(* No Lazy type nor Function type made. That's because function types will be at this point be converted into other types *)
			| _ -> if !strict_mode then begin trace ("[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"); assert false end else "[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"

	and param_t_s pos t =
		match run_follow gen t with
			| TAbstract ({ a_path = ([], "Bool") },[]) ->
					path_s_import pos (["java";"lang"], "Boolean") []
			| TAbstract ({ a_path = ([],"Float") },[]) ->
					path_s_import pos (["java";"lang"], "Double") []
			| TAbstract ({ a_path = ([],"Int") },[]) ->
					path_s_import pos (["java";"lang"], "Integer") []
			| TType ({ t_path = ["java"], "Int64" },[])
			| TAbstract ({ a_path = ["java"], "Int64" },[]) ->
					path_s_import pos (["java";"lang"], "Long") []
			| TInst ({ cl_path = ["haxe"],"Int64" },[])
			| TAbstract ({ a_path = ["haxe"],"Int64" },[]) ->
					path_s_import pos (["java";"lang"], "Long") []
			| TInst ({ cl_path = ["haxe"],"Int32" },[])
			| TAbstract ({ a_path = ["haxe"],"Int32" },[]) ->
					path_s_import pos (["java";"lang"], "Integer") []
			| TType ({ t_path = ["java"],"Int8" },[])
			| TAbstract ({ a_path = ["java"],"Int8" },[]) ->
					path_s_import pos (["java";"lang"], "Byte") []
			| TType ({ t_path = ["java"],"Int16" },[])
			| TAbstract ({ a_path = ["java"],"Int16" },[]) ->
					path_s_import pos (["java";"lang"], "Short") []
			| TType ({ t_path = ["java"],"Char16" },[])
			| TAbstract ({ a_path = ["java"],"Char16" },[]) ->
					path_s_import pos (["java";"lang"], "Character") []
			| TType ({ t_path = [],"Single" },[])
			| TAbstract ({ a_path = [],"Single" },[]) ->
					path_s_import pos (["java";"lang"], "Float") []
			| TDynamic _ -> "?"
			| TInst (cl, params) -> t_s pos (TInst(cl, change_param_type (TClassDecl cl) params))
			| TType (cl, params) -> t_s pos (TType(cl, change_param_type (TTypeDecl cl) params))
			| TEnum (e, params) -> t_s pos (TEnum(e, change_param_type (TEnumDecl e) params))
			| _ -> t_s pos t

	and path_param_s pos md path params meta =
			match params with
				| [] -> path_s_import pos path meta
				| _ when has_tdynamic (change_param_type md params) -> path_s_import pos path meta
				| _ -> sprintf "%s<%s>" (path_s_import pos path meta) (String.concat ", " (List.map (fun t -> param_t_s pos t) (change_param_type md params)))
	in

	let rett_s pos t =
		match t with
			| TAbstract ({ a_path = ([], "Void") },[]) -> "void"
			| _ -> t_s pos t
	in

	let high_surrogate c = (c lsr 10) + 0xD7C0 in
	let low_surrogate c = (c land 0x3FF) lor 0xDC00 in

	let escape ichar b =
		match ichar with
			| 92 (* \ *) -> Buffer.add_string b "\\\\"
			| 39 (* ' *) -> Buffer.add_string b "\\\'"
			| 34 -> Buffer.add_string b "\\\""
			| 13 (* \r *) -> Buffer.add_string b "\\r"
			| 10 (* \n *) -> Buffer.add_string b "\\n"
			| 9 (* \t *) -> Buffer.add_string b "\\t"
			| c when c < 32 || (c >= 127 && c <= 0xFFFF) -> Buffer.add_string b (Printf.sprintf "\\u%.4x" c)
			| c when c > 0xFFFF -> Buffer.add_string b (Printf.sprintf "\\u%.4x\\u%.4x" (high_surrogate c) (low_surrogate c))
			| c -> Buffer.add_char b (Char.chr c)
	in

	let escape s =
		let b = Buffer.create 0 in
		(try
			UTF8.validate s;
			UTF8.iter (fun c -> escape (UChar.code c) b) s
		with
			UTF8.Malformed_code ->
				String.iter (fun c -> escape (Char.code c) b) s
		);
		Buffer.contents b
	in

	let has_semicolon e =
		match e.eexpr with
			| TLocal { v_name = "__fallback__" }
			| TCall ({ eexpr = TLocal( { v_name = "__label__" } ) }, [ { eexpr = TConst(TInt _) } ] ) -> false
			| TCall ({ eexpr = TLocal( { v_name = "__lock__" } ) }, _ ) -> false
			| TBlock _ | TFor _ | TSwitch _ | TTry _ | TIf _ -> false
			| TWhile (_,_,flag) when flag = Ast.NormalWhile -> false
			| _ -> true
	in

	let in_value = ref false in

	let rec md_s pos md =
		let md = follow_module (gen.gfollow#run_f) md in
		match md with
			| TClassDecl (cl) ->
				t_s pos (TInst(cl,[]))
			| TEnumDecl (e) ->
				t_s pos (TEnum(e,[]))
			| TTypeDecl t ->
				t_s pos (TType(t, []))
			| TAbstractDecl a ->
				t_s pos (TAbstract(a, []))
	in

	(*
		it seems that Java doesn't like when you create a new array with the type parameter defined
		so we'll just ignore all type parameters, and hope for the best!
	*)
	let rec transform_nativearray_t t = match real_type t with
		| TInst( ({ cl_path = (["java"], "NativeArray") } as narr), [t]) ->
			TInst(narr, [transform_nativearray_t t])
		| TInst(cl, params) -> TInst(cl, List.map (fun _ -> t_dynamic) params)
		| TEnum(e, params) -> TEnum(e, List.map (fun _ -> t_dynamic) params)
		| TType(t, params) -> TType(t, List.map (fun _ -> t_dynamic) params)
		| _ -> t
	in

	let rec extract_tparams params el =
		match el with
			| ({ eexpr = TLocal({ v_name = "$type_param" }) } as tp) :: tl ->
				extract_tparams (tp.etype :: params) tl
			| _ -> (params, el)
	in

	let line_directive =
		if Common.defined gen.gcon Define.RealPosition then
			fun w p -> ()
		else fun w p ->
			let cur_line = Lexer.get_error_line p in
			let file = Path.get_full_path p.pfile in
			print w "//line %d \"%s\"" cur_line (Ast.s_escape file); newline w
	in

	let extract_statements expr =
		let ret = ref [] in
		let rec loop expr = match expr.eexpr with
			| TCall ({ eexpr = TLocal {
					v_name = "__is__" | "__typeof__" | "__array__"
				} }, el) ->
				List.iter loop el
			| TNew ({ cl_path = (["java"], "NativeArray") }, params, [ size ]) ->
				()
			| TUnop (Ast.Increment, _, _)
			| TUnop (Ast.Decrement, _, _)
			| TBinop (Ast.OpAssign, _, _)
			| TBinop (Ast.OpAssignOp _, _, _)
			| TLocal { v_name = "__fallback__" }
			| TLocal { v_name = "__sbreak__" } ->
				ret := expr :: !ret
			| TConst _
			| TLocal _
			| TArray _
			| TBinop _
			| TField _
			| TEnumParameter _
			| TTypeExpr _
			| TObjectDecl _
			| TArrayDecl _
			| TCast _
			| TMeta _
			| TParenthesis _
			| TUnop _ ->
				Type.iter loop expr
			| TFunction _ -> () (* do not extract parameters from inside of it *)
			| _ ->
				ret := expr :: !ret
		in
		loop expr;
		(* [expr] *)
		List.rev !ret
	in

	let expr_s w e =
		in_value := false;
		let rec expr_s w e =
			let was_in_value = !in_value in
			in_value := true;
			match e.eexpr with
				| TConst c ->
					(match c with
						| TInt i32 ->
							print w "%ld" i32;
						| TFloat s ->
							write w s;
							(* fix for Int notation, which only fit in a Float *)
							(if not (String.contains s '.' || String.contains s 'e' || String.contains s 'E') then write w ".0");
						| TString s -> print w "\"%s\"" (escape s)
						| TBool b -> write w (if b then "true" else "false")
						| TNull ->
							(match real_type e.etype with
								| TAbstract( { a_path = (["java"], "Int64") }, [] )
								| TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> write w "0L"
								| TInst( { cl_path = (["haxe"], "Int32") }, [] )
								| TAbstract ({ a_path = ([], "Int") },[]) -> expr_s w ({ e with eexpr = TConst(TInt Int32.zero) })
								| TAbstract ({ a_path = ([], "Float") },[]) -> expr_s w ({ e with eexpr = TConst(TFloat "0.0") })
								| TAbstract ({ a_path = ([], "Bool") },[]) -> write w "false"
								| TAbstract _ when like_int e.etype ->
									expr_s w (mk_cast e.etype { e with eexpr = TConst(TInt Int32.zero) })
								| TAbstract _ when like_float e.etype ->
									expr_s w (mk_cast e.etype { e with eexpr = TConst(TFloat "0.0") } )
								| t -> write w ("null") )
						| TThis -> write w "this"
						| TSuper -> write w "super")
				| TLocal { v_name = "__fallback__" } -> ()
				| TLocal { v_name = "__sbreak__" } -> write w "break"
				| TLocal { v_name = "__undefined__" } ->
					write w (t_s e.epos (TInst(runtime_cl, List.map (fun _ -> t_dynamic) runtime_cl.cl_params)));
					write w ".undefined";
				| TLocal var ->
					write_id w var.v_name
				| TField(_, FEnum(en,ef)) ->
					let s = ef.ef_name in
					print w "%s." (path_s_import e.epos en.e_path en.e_meta); write_field w s
				| TArray (e1, e2) ->
					expr_s w e1; write w "["; expr_s w e2; write w "]"
				| TBinop ((Ast.OpAssign as op), e1, e2)
				| TBinop ((Ast.OpAssignOp _ as op), e1, e2) ->
					expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2
				| TBinop (op, e1, e2) ->
					write w "( ";
					expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2;
					write w " )"
				| TField (e, FStatic(_, cf)) when Meta.has Meta.Native cf.cf_meta ->
					let rec loop meta = match meta with
						| (Meta.Native, [EConst (String s), _],_) :: _ ->
							expr_s w e; write w "."; write_field w s
						| _ :: tl -> loop tl
						| [] -> expr_s w e; write w "."; write_field w (cf.cf_name)
					in
					loop cf.cf_meta
				| TField (e, s) ->
					expr_s w e; write w "."; write_field w (field_name s)
				| TTypeExpr (TClassDecl { cl_path = (["haxe"], "Int32") }) ->
					write w (path_s_import e.epos (["haxe"], "Int32") [])
				| TTypeExpr (TClassDecl { cl_path = (["haxe"], "Int64") }) ->
					write w (path_s_import e.epos (["haxe"], "Int64") [])
				| TTypeExpr mt -> write w (md_s e.epos mt)
				| TParenthesis e ->
					write w "("; expr_s w e; write w ")"
				| TMeta (_,e) ->
					expr_s w e
				| TCall ({ eexpr = TLocal { v_name = "__array__" } }, el)
				| TCall ({ eexpr = TField(_, FStatic({ cl_path = (["java"],"NativeArray") }, { cf_name = "make" })) }, el)
				| TArrayDecl el when t_has_type_param e.etype ->
					let _, el = extract_tparams [] el in
					print w "( (%s) (new %s " (t_s e.epos e.etype) (t_s e.epos (replace_type_param e.etype));
					write w "{";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						expr_s w e;
						acc + 1
					) 0 el);
					write w "}) )"
				| TCall ({ eexpr = TLocal { v_name = "__array__" } }, el)
				| TCall ({ eexpr = TField(_, FStatic({ cl_path = (["java"],"NativeArray") }, { cf_name = "make" })) }, el)
				| TArrayDecl el ->
					let _, el = extract_tparams [] el in
					print w "new %s" (param_t_s e.epos (transform_nativearray_t e.etype));
					let is_double = match follow e.etype with
					 | TInst(_,[ t ]) -> if like_float t && not (like_int t) then Some t else None
					 | _ -> None
					in

					write w "{";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						(* this is a hack so we are able to convert ints to boxed Double / Float when needed *)
						let e = if is_some is_double then mk_cast (get is_double) e else e in

						expr_s w e;
						acc + 1
					) 0 el);
					write w "}"
				| TCall( ( { eexpr = TField(_, FStatic({ cl_path = ([], "String") }, { cf_name = "fromCharCode" })) } ), [cc] ) ->
						write w "Character.toString((char) ";
						expr_s w cc;
						write w ")"
				| TCall ({ eexpr = TLocal( { v_name = "__is__" } ) }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
					write w "( ";
					expr_s w expr;
					write w " instanceof ";
					write w (md_s e.epos md);
					write w " )"
				| TCall ({ eexpr = TLocal( { v_name = "__java__" } ) }, [ { eexpr = TConst(TString(s)) } ] ) ->
					write w s
				| TCall ({ eexpr = TLocal( { v_name = "__java__" } ) }, { eexpr = TConst(TString(s)) } :: tl ) ->
					Codegen.interpolate_code gen.gcon s tl (write w) (expr_s w) e.epos
				| TCall ({ eexpr = TLocal( { v_name = "__lock__" } ) }, [ eobj; eblock ] ) ->
					write w "synchronized(";
					let rec loop eobj = match eobj.eexpr with
						| TTypeExpr md ->
							expr_s w eobj;
							write w ".class"
						| TMeta(_,e) | TParenthesis(e) ->
							loop e
						| _ ->
							expr_s w eobj
					in
					loop eobj;
					write w ")";
					(match eblock.eexpr with
					| TBlock(_ :: _) ->
						expr_s w eblock
					| _ ->
						begin_block w;
						expr_s w eblock;
						if has_semicolon eblock then write w ";";
						end_block w;
					)
				| TCall ({ eexpr = TLocal( { v_name = "__goto__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
					print w "break label%ld" v
				| TCall ({ eexpr = TLocal( { v_name = "__label__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
					print w "label%ld:" v
				| TCall ({ eexpr = TLocal( { v_name = "__typeof__" } ) }, [ { eexpr = TTypeExpr md } as expr ] ) ->
					expr_s w expr;
					write w ".class"
				| TCall (e, el) ->
					let params, el = extract_tparams [] el in

					expr_s w e;

					(*(match params with
						| [] -> ()
						| params ->
							let md = match e.eexpr with
								| TField(ef, _) -> t_to_md (run_follow gen ef.etype)
								| _ -> assert false
							in
							write w "<";
							ignore (List.fold_left (fun acc t ->
								(if acc <> 0 then write w ", ");
								write w (param_t_s (change_param_type md t));
								acc + 1
							) 0 params);
							write w ">"
					);*)

					write w "(";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						expr_s w e;
						acc + 1
					) 0 el);
					write w ")"
				| TNew (({ cl_path = (["java"], "NativeArray") } as cl), params, [ size ]) ->
					let rec check_t_s t times =
						match real_type t with
							| TInst({ cl_path = (["java"], "NativeArray") }, [param]) ->
								(check_t_s param (times+1))
							| _ ->
								print w "new %s[" (t_s e.epos (transform_nativearray_t t));
								expr_s w size;
								print w "]";
								let rec loop i =
									if i <= 0 then () else (write w "[]"; loop (i-1))
								in
								loop (times - 1)
					in
					check_t_s (TInst(cl, params)) 0
				| TNew ({ cl_path = ([], "String") } as cl, [], el) ->
					write w "new ";
					write w (t_s e.epos (TInst(cl, [])));
					write w "(";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						expr_s w e;
						acc + 1
					) 0 el);
					write w ")"
				| TNew ({ cl_kind = KTypeParameter _ } as cl, params, el) ->
						print w "null /* This code should never be reached. It was produced by the use of @:generic on a new type parameter instance: %s */" (path_param_s e.epos (TClassDecl cl) cl.cl_path params cl.cl_meta)
				| TNew (cl, params, el) ->
					write w "new ";
					write w (path_param_s e.epos (TClassDecl cl) cl.cl_path params cl.cl_meta);
					write w "(";
					ignore (List.fold_left (fun acc e ->
						(if acc <> 0 then write w ", ");
						expr_s w e;
						acc + 1
					) 0 el);
					write w ")"
				| TUnop ((Ast.Increment as op), flag, e)
				| TUnop ((Ast.Decrement as op), flag, e) ->
					(match flag with
						| Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " " ); expr_s w e
						| Ast.Postfix -> expr_s w e; write w (Ast.s_unop op))
				| TUnop (op, flag, e) ->
					(match flag with
						| Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " (" ); expr_s w e; write w ") "
						| Ast.Postfix -> write w "("; expr_s w e; write w (") " ^ Ast.s_unop op))
				| TVar (var, eopt) ->
					print w "%s " (t_s e.epos var.v_type);
					write_id w var.v_name;
					(match eopt with
						| None ->
							write w " = ";
							expr_s w (null var.v_type e.epos)
						| Some e ->
							write w " = ";
							expr_s w e
					)
				| TBlock [e] when was_in_value ->
					expr_s w e
				| TBlock el ->
					begin_block w;
					List.iter (fun e ->
						List.iter (fun e ->
							in_value := false;
							line_directive w e.epos;
							expr_s w e;
							if has_semicolon e then write w ";";
							newline w;
						) (extract_statements e)
					) el;
					end_block w
				| TIf (econd, e1, Some(eelse)) when was_in_value ->
					write w "( ";
					expr_s w (mk_paren econd);
					write w " ? ";
					expr_s w (mk_paren e1);
					write w " : ";
					expr_s w (mk_paren eelse);
					write w " )";
				| TIf (econd, e1, eelse) ->
					write w "if ";
					expr_s w (mk_paren econd);
					write w " ";
					in_value := false;
					expr_s w (mk_block e1);
					(match eelse with
						| None -> ()
						| Some e ->
							write w "else";
							in_value := false;
							expr_s w (mk_block e)
					)
				| TWhile (econd, eblock, flag) ->
					(match flag with
						| Ast.NormalWhile ->
							write w "while ";
							expr_s w (mk_paren econd);
							write w "";
							in_value := false;
							expr_s w (mk_block eblock)
						| Ast.DoWhile ->
							write w "do ";
							in_value := false;
							expr_s w (mk_block eblock);
							write w "while ";
							in_value := true;
							expr_s w (mk_paren econd);
					)
				| TSwitch (econd, ele_l, default) ->
					write w "switch ";
					expr_s w (mk_paren econd);
					begin_block w;
					List.iter (fun (el, e) ->
						List.iter (fun e ->
							write w "case ";
							in_value := true;
							(match e.eexpr with
								| TField(_, FEnum(e, ef)) ->
									let changed_name = change_id ef.ef_name in
									write w changed_name
								| _ ->
									expr_s w e);
							write w ":";
							newline w;
						) el;
						in_value := false;
						expr_s w (mk_block e);
						newline w;
						newline w
					) ele_l;
					if is_some default then begin
						write w "default:";
						newline w;
						in_value := false;
						expr_s w (get default);
						newline w;
					end;
					end_block w
				| TTry (tryexpr, ve_l) ->
					write w "try ";
					in_value := false;
					expr_s w (mk_block tryexpr);
					let pos = e.epos in
					List.iter (fun (var, e) ->
						print w "catch (%s %s)" (t_s pos var.v_type) (var.v_name);
						in_value := false;
						expr_s w (mk_block e);
						newline w
					) ve_l
				| TReturn eopt ->
					write w "return ";
					if is_some eopt then expr_s w (get eopt)
				| TBreak -> write w "break"
				| TContinue -> write w "continue"
				| TThrow e ->
					write w "throw ";
					expr_s w e
				| TCast (e1,md_t) ->
					((*match gen.gfollow#run_f e.etype with
						| TType({ t_path = ([], "UInt") }, []) ->
							write w "( unchecked ((uint) ";
							expr_s w e1;
							write w ") )"
						| _ ->*)
							(* FIXME I'm ignoring module type *)
							print w "((%s) (" (t_s e.epos e.etype);
							expr_s w e1;
							write w ") )"
					)
				| TFor (_,_,content) ->
					write w "[ for not supported ";
					expr_s w content;
					write w " ]";
					if !strict_mode then assert false
				| TObjectDecl _ -> write w "[ obj decl not supported ]"; if !strict_mode then assert false
				| TFunction _ -> write w "[ func decl not supported ]"; if !strict_mode then assert false
				| TEnumParameter _ -> write w "[ enum parameter not supported ]"; if !strict_mode then assert false
		in
		expr_s w e
	in

	let rec gen_fpart_attrib w = function
		| EConst( Ident i ), _ ->
			write w i
		| EField( ef, f ), _ ->
			gen_fpart_attrib w ef;
			write w ".";
			write w f
		| _, p ->
			gen.gcon.error "Invalid expression inside @:meta metadata" p
	in

	let rec gen_spart w = function
		| EConst c, p -> (match c with
			| Int s | Float s | Ident s ->
				write w s
			| String s ->
				write w "\"";
				write w (escape s);
				write w "\""
			| _ -> gen.gcon.error "Invalid expression inside @:meta metadata" p)
		| EField( ef, f ), _ ->
			gen_spart w ef;
			write w ".";
			write w f
		| EBinop( Ast.OpAssign, (EConst (Ident s), _), e2 ), _ ->
			write w s;
			write w " = ";
			gen_spart w e2
		| EArrayDecl( el ), _ ->
			write w "{";
			let fst = ref true in
			List.iter (fun e ->
				if !fst then fst := false else write w ", ";
				gen_spart w e
			) el;
			write w "}"
		| ECall(fpart,args), _ ->
			gen_fpart_attrib w fpart;
			write w "(";
			let fst = ref true in
			List.iter (fun e ->
				if !fst then fst := false else write w ", ";
				gen_spart w e
			) args;
			write w ")"
		| _, p ->
			gen.gcon.error "Invalid expression inside @:meta metadata" p
	in

	let gen_annotations w ?(add_newline=true) metadata =
		List.iter (function
			| Meta.Meta, [meta], _ ->
				write w "@";
				gen_spart w meta;
				if add_newline then newline w else write w " ";
			| _ -> ()
		) metadata
	in

	let argt_s p t =
		let w = new_source_writer () in
		let rec run t =
			match t with
				| TType (tdef,p) ->
					gen_annotations w ~add_newline:false tdef.t_meta;
					run (follow_once t)
				| TMono r ->
					(match !r with
					| Some t -> run t
					| _ -> () (* avoid infinite loop / should be the same in this context *))
				| TLazy f ->
					run (!f())
				| _ -> ()
		in
		run t;
		let ret = t_s p t in
		let c = contents w in
		if c <> "" then
			c ^ " " ^ ret
		else
			ret
	in

	let get_string_params cl_params =
		match cl_params with
			| [] ->
				("","")
			| _ ->
				let params = sprintf "<%s>" (String.concat ", " (List.map (fun (_, tcl) -> match follow tcl with | TInst(cl, _) -> snd cl.cl_path | _ -> assert false) cl_params)) in
				let params_extends = List.fold_left (fun acc (name, t) ->
					match run_follow gen t with
						| TInst (cl, p) ->
							(match cl.cl_implements with
								| [] -> acc
								| _ -> acc) (* TODO
								| _ -> (sprintf " where %s : %s" name (String.concat ", " (List.map (fun (cl,p) -> path_param_s (TClassDecl cl) cl.cl_path p) cl.cl_implements))) :: acc ) *)
						| _ -> trace (t_s Ast.null_pos t); assert false (* FIXME it seems that a cl_params will never be anything other than cl.cl_params. I'll take the risk and fail if not, just to see if that confirms *)
				) [] cl_params in
				(params, String.concat " " params_extends)
	in

	let write_parts w parts =
		let parts = List.filter (fun s -> s <> "") parts in
		write w (String.concat " " parts)
	in

	let rec gen_class_field w ?(is_overload=false) is_static cl is_final cf =
		let is_interface = cl.cl_interface in
		let name, is_new, is_explicit_iface = match cf.cf_name with
			| "new" -> snd cl.cl_path, true, false
			| name when String.contains name '.' ->
				let fn_name, path = parse_explicit_iface name in
				(path_s path cl.cl_meta) ^ "." ^ fn_name, false, true
			| name -> name, false, false
		in
		(match cf.cf_kind with
			| Var _
			| Method (MethDynamic) when not (Type.is_extern_field cf) ->
				(if is_overload || List.exists (fun cf -> cf.cf_expr <> None) cf.cf_overloads then
					gen.gcon.error "Only normal (non-dynamic) methods can be overloaded" cf.cf_pos);
				if not is_interface then begin
					let access, modifiers = get_fun_modifiers cf.cf_meta "public" [] in
					write_parts w (access :: (if is_static then "static" else "") :: modifiers @ [(t_s cf.cf_pos (run_follow gen cf.cf_type)); (change_field name)]);
					(match cf.cf_expr with
						| Some e ->
								write w " = ";
								expr_s w e;
								write w ";"
						| None -> write w ";"
					)
				end (* TODO see how (get,set) variable handle when they are interfaces *)
			| Method _ when Type.is_extern_field cf || (match cl.cl_kind, cf.cf_expr with | KAbstractImpl _, None -> true | _ -> false) ->
				List.iter (fun cf -> if cl.cl_interface || cf.cf_expr <> None then
					gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
				) cf.cf_overloads
			| Var _ | Method MethDynamic -> ()
			| Method mkind ->
				List.iter (fun cf ->
					if cl.cl_interface || cf.cf_expr <> None then
						gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
				) cf.cf_overloads;
				let is_virtual = is_new || (not is_final && match mkind with | MethInline -> false | _ when not is_new -> true | _ -> false) in
				let is_override = match cf.cf_name with
					| "equals" when not is_static ->
						(match cf.cf_type with
							| TFun([_,_,t], ret) ->
								(match (real_type t, real_type ret) with
									| TDynamic _, TAbstract ({ a_path = ([], "Bool") },[])
									| TAnon _, TAbstract ({ a_path = ([], "Bool") },[]) -> true
									| _ -> List.memq cf cl.cl_overrides
								)
							| _ -> List.memq cf cl.cl_overrides)
					| "toString" when not is_static ->
						(match cf.cf_type with
							| TFun([], ret) ->
								(match real_type ret with
									| TInst( { cl_path = ([], "String") }, []) -> true
									| _ -> gen.gcon.error "A toString() function should return a String!" cf.cf_pos; false
								)
							| _ -> List.memq cf cl.cl_overrides
						)
					| "hashCode" when not is_static ->
						(match cf.cf_type with
							| TFun([], ret) ->
								(match real_type ret with
									| TAbstract ({ a_path = ([], "Int") },[]) ->
										true
									| _ -> gen.gcon.error "A hashCode() function should return an Int!" cf.cf_pos; false
								)
							| _ -> List.memq cf cl.cl_overrides
						)
					| _ -> List.memq cf cl.cl_overrides
				in
				let visibility = if is_interface then "" else "public" in

				let visibility, modifiers = get_fun_modifiers cf.cf_meta visibility [] in
				let visibility, is_virtual = if is_explicit_iface then "",false else visibility, is_virtual in
				let v_n = if is_static then "static" else if is_override && not is_interface then "" else if not is_virtual then "final" else "" in
				let cf_type = if is_override && not is_overload && not (Meta.has Meta.Overload cf.cf_meta) then match field_access gen (TInst(cl, List.map snd cl.cl_params)) cf.cf_name with | FClassField(_,_,_,_,_,actual_t,_) -> actual_t | _ -> assert false else cf.cf_type in

				let params = List.map snd cl.cl_params in
				let ret_type, args = match follow cf_type, follow cf.cf_type with
					| TFun (strbtl, t), TFun(rargs, _) ->
							(apply_params cl.cl_params params (real_type t), List.map2 (fun(_,_,t) (n,o,_) -> (n,o,apply_params cl.cl_params params (real_type t))) strbtl rargs)
					| _ -> assert false
				in

				(if is_override && not is_interface then write w "@Override ");
				gen_annotations w cf.cf_meta;
				(* public static void funcName *)
				let params, _ = get_string_params cf.cf_params in

				write_parts w (visibility :: v_n :: modifiers @ [params; (if is_new then "" else rett_s cf.cf_pos (run_follow gen ret_type)); (change_field name)]);

				(* <T>(string arg1, object arg2) with T : object *)
				(match cf.cf_expr with
					| Some { eexpr = TFunction tf } ->
							print w "(%s)" (String.concat ", " (List.map2 (fun (var,_) (_,_,t) -> sprintf "%s %s" (argt_s cf.cf_pos (run_follow gen t)) (change_id var.v_name)) tf.tf_args args))
					| _ ->
							print w "(%s)" (String.concat ", " (List.map (fun (name, _, t) -> sprintf "%s %s" (argt_s cf.cf_pos (run_follow gen t)) (change_id name)) args))
				);
				if is_interface || List.mem "native" modifiers then
					write w ";"
				else begin
					let rec loop meta =
						match meta with
							| [] ->
								let expr = match cf.cf_expr with
									| None -> mk (TBlock([])) t_dynamic Ast.null_pos
									| Some s ->
										match s.eexpr with
											| TFunction tf ->
												mk_block (tf.tf_expr)
											| _ -> assert false (* FIXME *)
								in
								(if is_new then begin
									(*let rec get_super_call el =
										match el with
											| ( { eexpr = TCall( { eexpr = TConst(TSuper) }, _) } as call) :: rest ->
												Some call, rest
											| ( { eexpr = TBlock(bl) } as block ) :: rest ->
												let ret, mapped = get_super_call bl in
												ret, ( { block with eexpr = TBlock(mapped) } :: rest )
											| _ ->
												None, el
									in*)
									expr_s w expr
								end else begin
									expr_s w expr;
								end)
							| (Meta.Throws, [Ast.EConst (Ast.String t), _], _) :: tl ->
								print w " throws %s" t;
								loop tl
							| (Meta.FunctionCode, [Ast.EConst (Ast.String contents),_],_) :: tl ->
								begin_block w;
								write w contents;
								end_block w
							| _ :: tl -> loop tl
					in
					loop cf.cf_meta

				end);
			newline w;
			newline w
	in

	let gen_class w cl =
		let cf_filters = [ handle_throws ] in
		List.iter (fun f -> List.iter (f gen) cl.cl_ordered_fields) cf_filters;
		List.iter (fun f -> List.iter (f gen) cl.cl_ordered_statics) cf_filters;
		let should_close = match change_ns (fst cl.cl_path) with
			| [] -> false
			| ns ->
				print w "package %s;" (String.concat "." (change_ns ns));
				newline w;
				newline w;
				false
		in

		let rec loop_meta meta acc =
			match meta with
				| (Meta.SuppressWarnings, [Ast.EConst (Ast.String w),_],_) :: meta -> loop_meta meta (w :: acc)
				| _ :: meta -> loop_meta meta acc
				| _ -> acc
		in

		let suppress_warnings = loop_meta cl.cl_meta [ "rawtypes"; "unchecked" ] in

		write w "import haxe.root.*;";
		newline w;
		let w_header = w in
		let w = new_source_writer () in
		clear_scope();

		(* add all haxe.root.* to imports *)
		List.iter (function
			| TClassDecl { cl_path = ([],c) } ->
					imports := ([],c) :: !imports
			| TEnumDecl { e_path = ([],c) } ->
					imports := ([],c) :: !imports
			| TAbstractDecl { a_path = ([],c) } ->
					imports := ([],c) :: !imports
			| _ -> ()
		) gen.gtypes_list;

		newline w;
		write w "@SuppressWarnings(value={";
		let first = ref true in
		List.iter (fun s ->
			(if !first then first := false else write w ", ");
			print w "\"%s\"" (escape s)
		) suppress_warnings;
		write w "})";
		newline w;
		gen_annotations w cl.cl_meta;

		let clt, access, modifiers = get_class_modifiers cl.cl_meta (if cl.cl_interface then "interface" else "class") "public" [] in
		let is_final = Meta.has Meta.Final cl.cl_meta in

		write_parts w (access :: modifiers @ [clt; (change_clname (snd cl.cl_path))]);

		(* type parameters *)
		let params, _ = get_string_params cl.cl_params in
		let cl_p_to_string (c,p) =
			let p = List.map (fun t -> match follow t with
				| TMono _ | TDynamic _ -> t_empty
				| _ -> t) p
			in
			path_param_s cl.cl_pos (TClassDecl c) c.cl_path p c.cl_meta
		in
		print w "%s" params;
		(if is_some cl.cl_super then print w " extends %s" (cl_p_to_string (get cl.cl_super)));
		(match cl.cl_implements with
			| [] -> ()
			| _ -> print w " %s %s" (if cl.cl_interface then "extends" else "implements") (String.concat ", " (List.map cl_p_to_string cl.cl_implements))
		);
		(* class head ok: *)
		(* public class Test<A> : X, Y, Z where A : Y *)
		begin_block w;
		(* our constructor is expected to be a normal "new" function *
		if !strict_mode && is_some cl.cl_constructor then assert false;*)

		let rec loop cl =
			List.iter (fun cf -> add_scope cf.cf_name) cl.cl_ordered_fields;
			List.iter (fun cf -> add_scope cf.cf_name) cl.cl_ordered_statics;
			match cl.cl_super with
				| Some(c,_) -> loop c
				| None -> ()
		in
		loop cl;

		let rec loop meta =
			match meta with
				| [] -> ()
				| (Meta.ClassCode, [Ast.EConst (Ast.String contents),_],_) :: tl ->
					write w contents
				| _ :: tl -> loop tl
		in
		loop cl.cl_meta;

		(match gen.gcon.main_class with
			| Some path when path = cl.cl_path ->
				write w "public static void main(String[] args)";
				begin_block w;
				(try
					let t = Hashtbl.find gen.gtypes ([], "Sys") in
							match t with
								| TClassDecl(cl) when PMap.mem "_args" cl.cl_statics ->
									write w "Sys._args = args;"; newline w
								| _ -> ()
				with | Not_found -> ()
				);
				write w "main();";
				end_block w;
				newline w
			| _ -> ()
		);

		(match cl.cl_init with
			| None -> ()
			| Some init ->
				write w "static";
				expr_s w (mk_block init);
				newline w
		);

		(if is_some cl.cl_constructor then gen_class_field w false cl is_final (get cl.cl_constructor));
		(if not cl.cl_interface then List.iter (gen_class_field w true cl is_final) cl.cl_ordered_statics);
		List.iter (gen_class_field w false cl is_final) cl.cl_ordered_fields;

		end_block w;
		if should_close then end_block w;

		(* add imports *)
		List.iter (function
			| ["haxe";"root"], _ | [], _ -> ()
			| path ->
					write w_header "import ";
					write w_header (path_s path []);
					write w_header ";\n"
		) !imports;
		add_writer w w_header
	in


	let gen_enum w e =
		let should_close = match change_ns (fst e.e_path) with
			| [] -> false
			| ns ->
				print w "package %s;" (String.concat "." (change_ns ns));
				newline w;
				newline w;
				false
		in

		gen_annotations w e.e_meta;
		print w "public enum %s" (change_clname (snd e.e_path));
		begin_block w;
		write w (String.concat ", " (List.map (change_id) e.e_names));
		end_block w;

		if should_close then end_block w
	in

	let module_type_gen w md_tp =
		Codegen.map_source_header gen.gcon (fun s -> print w "// %s\n" s);
		match md_tp with
			| TClassDecl cl ->
				if not cl.cl_extern then begin
					gen_class w cl;
					newline w;
					newline w
				end;
				(not cl.cl_extern)
			| TEnumDecl e ->
				if not e.e_extern && not (Meta.has Meta.Class e.e_meta) then begin
					gen_enum w e;
					newline w;
					newline w
				end;
				(not e.e_extern)
			| TTypeDecl e ->
				false
			| TAbstractDecl a ->
				false
	in

	(* generate source code *)
	init_ctx gen;

	Hashtbl.add gen.gspecial_vars "__label__" true;
	Hashtbl.add gen.gspecial_vars "__goto__" true;
	Hashtbl.add gen.gspecial_vars "__is__" true;
	Hashtbl.add gen.gspecial_vars "__typeof__" true;
	Hashtbl.add gen.gspecial_vars "__java__" true;
	Hashtbl.add gen.gspecial_vars "__lock__" true;
	Hashtbl.add gen.gspecial_vars "__array__" true;

	gen.greal_type <- real_type;
	gen.greal_type_param <- change_param_type;

	SetHXGen.run_filter gen;

	(* before running the filters, follow all possible types *)
	(* this is needed so our module transformations don't break some core features *)
	(* like multitype selection *)
	let run_follow_gen = run_follow gen in
	let rec type_map e = Type.map_expr_type (fun e->type_map e) (run_follow_gen)	(fun tvar-> tvar.v_type <- (run_follow_gen tvar.v_type); tvar) e in
	let super_map (cl,tl) = (cl, List.map run_follow_gen tl) in
	List.iter (function
		| TClassDecl cl ->
				let all_fields = (Option.map_default (fun cf -> [cf]) [] cl.cl_constructor) @ cl.cl_ordered_fields @ cl.cl_ordered_statics in
				List.iter (fun cf ->
					cf.cf_type <- run_follow_gen cf.cf_type;
					cf.cf_expr <- Option.map type_map cf.cf_expr
				) all_fields;
			 cl.cl_dynamic <- Option.map run_follow_gen cl.cl_dynamic;
			 cl.cl_array_access <- Option.map run_follow_gen cl.cl_array_access;
			 cl.cl_init <- Option.map type_map cl.cl_init;
			 cl.cl_super <- Option.map super_map cl.cl_super;
			 cl.cl_implements <- List.map super_map cl.cl_implements
		| _ -> ()
		) gen.gtypes_list;

	let get_vmtype t = match real_type t with
		| TInst({ cl_path = ["java"],"NativeArray" }, tl) -> t
		| TInst(c,tl) -> TInst(c,List.map (fun _ -> t_dynamic) tl)
		| TEnum(e,tl) -> TEnum(e, List.map (fun _ -> t_dynamic) tl)
		| TType(t,tl) -> TType(t, List.map (fun _ -> t_dynamic) tl)
		| TAbstract(a,tl) -> TAbstract(a, List.map (fun _ -> t_dynamic) tl)
		| t -> t
	in

	FixOverrides.configure ~get_vmtype gen;
	Normalize.configure gen ~metas:(Hashtbl.create 0);
	AbstractImplementationFix.configure gen;

	IteratorsInterface.configure gen;

	let closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx gen (get_cl (get_type gen (["haxe";"lang"],"Function"))) 6 in
	ClosuresToClass.configure gen closure_t;

	let enum_base = (get_cl (get_type gen (["haxe";"lang"],"Enum")) ) in
	let param_enum_base = (get_cl (get_type gen (["haxe";"lang"],"ParamEnum")) ) in
	EnumToClass.configure gen (None) false true enum_base param_enum_base;

	InterfaceVarsDeleteModf.configure gen;

	let dynamic_object = (get_cl (get_type gen (["haxe";"lang"],"DynamicObject")) ) in

	let object_iface = get_cl (get_type gen (["haxe";"lang"],"IHxObject")) in

	let empty_en = match get_type gen (["haxe";"lang"], "EmptyObject") with TEnumDecl e -> e | _ -> assert false in
	let empty_ctor_type = TEnum(empty_en, []) in
	let empty_en_expr = mk (TTypeExpr (TEnumDecl empty_en)) (TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics empty_en) }) null_pos in
	let empty_ctor_expr = mk (TField (empty_en_expr, FEnum(empty_en, PMap.find "EMPTY" empty_en.e_constrs))) empty_ctor_type null_pos in
	OverloadingConstructor.configure ~empty_ctor_type:empty_ctor_type ~empty_ctor_expr:empty_ctor_expr gen;

	let rcf_static_find = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "findHash" Ast.null_pos [] in
	(*let rcf_static_lookup = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "lookupHash" Ast.null_pos [] in*)
	let get_specialized_postfix t = match t with
		| TAbstract({a_path = [],"Float"}, _) -> "Float"
		| TInst({cl_path = [],"String"},_) -> "String"
		| TAnon _ | TDynamic _ -> "Dynamic"
		| _ -> print_endline (debug_type t); assert false
	in
	let rcf_static_insert t = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) ("insert" ^ get_specialized_postfix t) Ast.null_pos [] in
	let rcf_static_remove t = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) ("remove" ^ get_specialized_postfix t) Ast.null_pos [] in

	let can_be_float t = like_float (real_type t) in

	let rcf_on_getset_field main_expr field_expr field may_hash may_set is_unsafe =
		let is_float = can_be_float (if is_none may_set then main_expr.etype else (get may_set).etype) in
		let fn_name = if is_some may_set then "setField" else "getField" in
		let fn_name = if is_float then fn_name ^ "_f" else fn_name in
		let pos = field_expr.epos in

		let is_unsafe = { eexpr = TConst(TBool is_unsafe); etype = basic.tbool; epos = pos } in

		let should_cast = match main_expr.etype with | TAbstract({ a_path = ([], "Float") }, []) -> false | _ -> true in
		let infer = mk_static_field_access_infer runtime_cl fn_name field_expr.epos [] in
		let first_args =
			[ field_expr; { eexpr = TConst(TString field); etype = basic.tstring; epos = pos } ]
			@ if is_some may_hash then [ { eexpr = TConst(TInt (get may_hash)); etype = basic.tint; epos = pos } ] else []
		in
		let args = first_args @ match is_float, may_set with
			| true, Some(set) ->
				[ if should_cast then mk_cast basic.tfloat set else set ]
			| false, Some(set) ->
				[ set ]
			| _ ->
				[ is_unsafe ]
		in

		let call = { main_expr with eexpr = TCall(infer,args) } in
		let call = if is_float && should_cast then mk_cast main_expr.etype call else call in
		call
	in

	let rcf_on_call_field ecall field_expr field may_hash args =
		let infer = mk_static_field_access_infer runtime_cl "callField" field_expr.epos [] in

		let hash_arg = match may_hash with
			| None -> []
			| Some h -> [ { eexpr = TConst(TInt h); etype = basic.tint; epos = field_expr.epos } ]
		in

		let arr_call = if args <> [] then
			{ eexpr = TArrayDecl args; etype = basic.tarray t_dynamic; epos = ecall.epos }
		else
			null (basic.tarray t_dynamic) ecall.epos
		in


		let call_args =
			[field_expr; { field_expr with eexpr = TConst(TString field); etype = basic.tstring } ]
				@ hash_arg
				@ [ arr_call ]
		in

		mk_cast ecall.etype { ecall with eexpr = TCall(infer, call_args); etype = t_dynamic }
	in

	let rcf_ctx =
		ReflectionCFs.new_ctx
			gen
			closure_t
			object_iface
			false
			rcf_on_getset_field
			rcf_on_call_field
			(fun hash hash_array length -> { hash with eexpr = TCall(rcf_static_find, [hash; hash_array; length]); etype=basic.tint })
			(fun hash -> hash)
			(fun hash_array length pos value ->
				{ hash_array with
					eexpr = TBinop(OpAssign,
								hash_array,
								mk (TCall(rcf_static_insert value.etype, [hash_array; length; pos; value])) hash_array.etype hash_array.epos)
			})
			(fun hash_array length pos ->
				let t = gen.gclasses.nativearray_type hash_array.etype in
				{ hash_array with eexpr = TCall(rcf_static_remove t, [hash_array; length; pos]); etype = gen.gcon.basic.tvoid }
			)
			None
		in

	ReflectionCFs.UniversalBaseClass.configure gen (get_cl (get_type gen (["haxe";"lang"],"HxObject")) ) object_iface dynamic_object;

	ReflectionCFs.configure_dynamic_field_access rcf_ctx;

	ReflectionCFs.implement_varargs_cl rcf_ctx ( get_cl (get_type gen (["haxe";"lang"], "VarArgsBase")) );

	let slow_invoke = mk_static_field_access_infer (runtime_cl) "slowCallField" Ast.null_pos [] in
	ReflectionCFs.configure rcf_ctx ~slow_invoke:(fun ethis efield eargs -> {
		eexpr = TCall(slow_invoke, [ethis; efield; eargs]);
		etype = t_dynamic;
		epos = ethis.epos;
	} ) object_iface;

	ObjectDeclMap.configure gen (ReflectionCFs.implement_dynamic_object_ctor rcf_ctx dynamic_object);

	InitFunction.configure gen;
	TArrayTransform.configure gen (
	fun e _ ->
		match e.eexpr with
			| TArray ({ eexpr = TLocal { v_extra = Some( _ :: _, _) } }, _) -> (* captured transformation *)
				false
			| TArray(e1, e2) ->
				( match run_follow gen (follow e1.etype) with
					| TInst({ cl_path = (["java"], "NativeArray") }, _) -> false
					| _ -> true )
			| _ -> assert false
	) "__get" "__set";

	let field_is_dynamic t field =
		match field_access_esp gen (gen.greal_type t) field with
			| FClassField (cl,p,_,_,_,t,_) ->
				let p = change_param_type (TClassDecl cl) p in
				is_dynamic (apply_params cl.cl_params p t)
			| FEnumField _ -> false
			| _ -> true
	in

	let is_type_param e = match follow e with
		| TInst( { cl_kind = KTypeParameter _ },[]) -> true
		| _ -> false
	in

	let is_dynamic_expr e =
		is_dynamic e.etype || match e.eexpr with
		| TField(tf, f) ->
			field_is_dynamic tf.etype f
		| _ ->
			false
	in

	let may_nullable t = match gen.gfollow#run_f t with
		| TType({ t_path = ([], "Null") }, [t]) ->
			(match follow t with
				| TInst({ cl_path = ([], "String") }, [])
				| TAbstract ({ a_path = ([], "Float") },[])
				| TInst({ cl_path = (["haxe"], "Int32")}, [] )
				| TInst({ cl_path = (["haxe"], "Int64")}, [] )
				| TAbstract ({ a_path = ([], "Int") },[])
				| TAbstract ({ a_path = ([], "Bool") },[]) -> Some t
				| t when is_java_basic_type t -> Some t
				| _ -> None )
		| _ -> None
	in

	let is_double t = like_float t && not (like_int t) in
	let is_int t = like_int t in

	DynamicOperators.configure gen
		(fun e -> match e.eexpr with
			| TBinop (Ast.OpEq, e1, e2) ->
				is_dynamic e1.etype || is_dynamic e2.etype || is_type_param e1.etype || is_type_param e2.etype
			| TBinop (Ast.OpAdd, e1, e2)
			| TBinop (Ast.OpNotEq, e1, e2) -> is_dynamic e1.etype || is_dynamic e2.etype || is_type_param e1.etype || is_type_param e2.etype
			| TBinop (Ast.OpLt, e1, e2)
			| TBinop (Ast.OpLte, e1, e2)
			| TBinop (Ast.OpGte, e1, e2)
			| TBinop (Ast.OpGt, e1, e2) -> is_dynamic e.etype || is_dynamic_expr e1 || is_dynamic_expr e2 || is_string e1.etype || is_string e2.etype
			| TBinop (_, e1, e2) -> is_dynamic e.etype || is_dynamic_expr e1 || is_dynamic_expr e2
			| TUnop (_, _, e1) ->
				is_dynamic_expr e1
			| _ -> false)
		(fun e1 e2 ->
			let is_null e = match e.eexpr with | TConst(TNull) | TLocal({ v_name = "__undefined__" }) -> true | _ -> false in

			match e1.eexpr, e2.eexpr with
				| TConst c1, TConst c2 when is_null e1 || is_null e2 ->
					{ e1 with eexpr = TConst(TBool (c1 = c2)); etype = basic.tbool }
				| _ when is_null e1 || is_null e2 && not (is_java_basic_type e1.etype || is_java_basic_type e2.etype) ->
					{ e1 with eexpr = TBinop(Ast.OpEq, e1, e2); etype = basic.tbool }
				| _ ->
				let is_ref = match follow e1.etype, follow e2.etype with
					| TDynamic _, _
					| _, TDynamic _
					| TAbstract ({ a_path = ([], "Float") },[]) , _
					| TInst( { cl_path = (["haxe"], "Int32") }, [] ), _
					| TInst( { cl_path = (["haxe"], "Int64") }, [] ), _
					| TAbstract ({ a_path = ([], "Int") },[]) , _
					| TAbstract ({ a_path = ([], "Bool") },[]) , _
					| _, TAbstract ({ a_path = ([], "Float") },[])
					| _, TAbstract ({ a_path = ([], "Int") },[])
					| _, TInst( { cl_path = (["haxe"], "Int32") }, [] )
					| _, TInst( { cl_path = (["haxe"], "Int64") }, [] )
					| _, TAbstract ({ a_path = ([], "Bool") },[])
					| TInst( { cl_kind = KTypeParameter _ }, [] ), _
					| _, TInst( { cl_kind = KTypeParameter _ }, [] ) -> false
					| _, _ -> true
				in

				let static = mk_static_field_access_infer (runtime_cl) (if is_ref then "refEq" else "eq") e1.epos [] in
				{ eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tbool; epos=e1.epos }
		)
		(fun e e1 e2 ->
			match may_nullable e1.etype, may_nullable e2.etype with
				| Some t1, Some t2 ->
					let t1, t2 = if is_string t1 || is_string t2 then
						basic.tstring, basic.tstring
					else if is_double t1 || is_double t2 then
						basic.tfloat, basic.tfloat
					else if is_int t1 || is_int t2 then
						basic.tint, basic.tint
					else t1, t2 in
					{ eexpr = TBinop(Ast.OpAdd, mk_cast t1 e1, mk_cast t2 e2); etype = e.etype; epos = e1.epos }
				| _ ->
					let static = mk_static_field_access_infer (runtime_cl) "plus"  e1.epos [] in
					mk_cast e.etype { eexpr = TCall(static, [e1; e2]); etype = t_dynamic; epos=e1.epos })
		(fun e1 e2 ->
			if is_string e1.etype then begin
				{ e1 with eexpr = TCall(mk_field_access gen e1 "compareTo" e1.epos, [ e2 ]); etype = gen.gcon.basic.tint }
			end else begin
				let static = mk_static_field_access_infer (runtime_cl) "compare" e1.epos [] in
				{ eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tint; epos=e1.epos }
			end);

	let closure_cl = get_cl (get_type gen (["haxe";"lang"],"Closure")) in
	FilterClosures.configure gen (fun e1 s -> true) (ReflectionCFs.get_closure_func rcf_ctx closure_cl);

	let base_exception = get_cl (get_type gen (["java"; "lang"], "Throwable")) in
	let base_exception_t = TInst(base_exception, []) in

	let hx_exception = get_cl (get_type gen (["haxe";"lang"], "HaxeException")) in
	let hx_exception_t = TInst(hx_exception, []) in

	let rec is_exception t =
		match follow t with
			| TInst(cl,_) ->
				if cl == base_exception then
					true
				else
					(match cl.cl_super with | None -> false | Some (cl,arg) -> is_exception (TInst(cl,arg)))
			| _ -> false
	in

	TryCatchWrapper.configure gen
		(fun t -> not (is_exception (real_type t)))
		(fun throwexpr expr ->
			let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], hx_exception_t)) expr.epos in
			{ throwexpr with eexpr = TThrow { expr with eexpr = TCall(wrap_static, [expr]); etype = hx_exception_t }; etype = gen.gcon.basic.tvoid }
		)
		(fun v_to_unwrap pos ->
			let local = mk_cast hx_exception_t { eexpr = TLocal(v_to_unwrap); etype = v_to_unwrap.v_type; epos = pos } in
			mk_field_access gen local "obj" pos
		)
		(fun rethrow ->
			let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], hx_exception_t)) rethrow.epos in
			{ rethrow with eexpr = TThrow { rethrow with eexpr = TCall(wrap_static, [rethrow]); etype = hx_exception_t }; }
		)
		(base_exception_t)
		(hx_exception_t)
		(fun v e ->

			let exc_cl = get_cl (get_type gen (["haxe";"lang"],"Exceptions")) in
			let exc_field = mk_static_field_access_infer exc_cl "setException" e.epos [] in
			let esetstack = { eexpr = TCall(exc_field,[mk_local v e.epos]); etype = gen.gcon.basic.tvoid; epos = e.epos } in

			Type.concat esetstack e;
		);

	ClassInstance.configure gen (fun e _ -> { e with eexpr = TCall({ eexpr = TLocal(alloc_var "__typeof__" t_dynamic); etype = t_dynamic; epos = e.epos }, [e]) });

	CastDetect.configure gen (Some empty_ctor_type) false;

	SwitchToIf.configure gen (fun e ->
		match e.eexpr with
			| TSwitch(cond, cases, def) ->
				(match gen.gfollow#run_f cond.etype with
					| TInst( { cl_path = (["haxe"], "Int32") }, [] )
					| TAbstract ({ a_path = ([], "Int") },[])
					| TInst({ cl_path = ([], "String") },[]) ->
						(List.exists (fun (c,_) ->
							List.exists (fun expr -> match expr.eexpr with | TConst _ -> false | _ -> true ) c
						) cases)
					| _ -> true
				)
			| _ -> assert false
	);

	ExpressionUnwrap.configure gen (fun e -> Some { eexpr = TVar(mk_temp gen "expr" e.etype, Some e); etype = gen.gcon.basic.tvoid; epos = e.epos });

	UnnecessaryCastsRemoval.configure gen;

	IntDivisionSynf.configure gen;

	UnreachableCodeEliminationSynf.configure gen true;

	ArrayDeclSynf.configure gen native_arr_cl;

	let goto_special = alloc_var "__goto__" t_dynamic in
	let label_special = alloc_var "__label__" t_dynamic in
	SwitchBreakSynf.configure gen
		(fun e_loop n api ->
			{ e_loop with eexpr = TBlock( { eexpr = TCall( mk_local label_special e_loop.epos, [ Codegen.ExprBuilder.make_int gen.gcon n e_loop.epos ] ); etype = t_dynamic; epos = e_loop.epos } :: [e_loop] ) };
		)
		(fun e_break n api ->
			{ eexpr = TCall( mk_local goto_special e_break.epos, [  Codegen.ExprBuilder.make_int gen.gcon n e_break.epos ] ); etype = t_dynamic; epos = e_break.epos }
		);

	DefaultArguments.configure gen;
	InterfaceMetas.configure gen;

	JavaSpecificSynf.configure gen runtime_cl;
	JavaSpecificESynf.configure gen runtime_cl;

	(* add native String as a String superclass *)
	let str_cl = match gen.gcon.basic.tstring with | TInst(cl,_) -> cl | _ -> assert false in
	str_cl.cl_super <- Some (get_cl (get_type gen (["haxe";"lang"], "NativeString")), []);

	let mkdir dir = if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 in
	mkdir gen.gcon.file;
	mkdir (gen.gcon.file ^ "/src");

	let out_files = ref [] in

	(* add resources array *)
	let res = ref [] in
	Hashtbl.iter (fun name v ->
		res := { eexpr = TConst(TString name); etype = gen.gcon.basic.tstring; epos = Ast.null_pos } :: !res;
		let name = Codegen.escape_res_name name true in
		let full_path = gen.gcon.file ^ "/src/" ^ name in
		mkdir_from_path full_path;

		let f = open_out_bin full_path in
		output_string f v;
		close_out f;

		out_files := (Path.unique_full_path full_path) :: !out_files
	) gen.gcon.resources;
	(try
		let c = get_cl (Hashtbl.find gen.gtypes (["haxe"], "Resource")) in
		let cf = PMap.find "content" c.cl_statics in
		cf.cf_expr <- Some ({ eexpr = TArrayDecl(!res); etype = gen.gcon.basic.tarray gen.gcon.basic.tstring; epos = Ast.null_pos })
	with | Not_found -> ());

	run_filters gen;

	TypeParams.RenameTypeParameters.run gen;

	let parts = Str.split_delim (Str.regexp "[\\/]+") gen.gcon.file in
	mkdir_recursive "" parts;

	let source_dir = gen.gcon.file ^ "/src" in
	List.iter (fun md ->
		let w = SourceWriter.new_source_writer () in
		let should_write = module_type_gen w md in
		if should_write then begin
			let path = change_path (t_path md) in
			write_file gen w (source_dir ^ "/" ^ (String.concat "/" (fst path))) path "java" out_files;
		end
	) gen.gtypes_list;

	if not (Common.defined gen.gcon Define.KeepOldOutput) then
		clean_files (gen.gcon.file ^ "/src") !out_files gen.gcon.verbose;

	let path_s_desc path = path_s path [] in
	dump_descriptor gen ("hxjava_build.txt") path_s_desc (fun md -> path_s_desc (t_infos md).mt_path);
	if ( not (Common.defined gen.gcon Define.NoCompilation) ) then begin
		let old_dir = Sys.getcwd() in
		Sys.chdir gen.gcon.file;
		let cmd = "haxelib run hxjava hxjava_build.txt --haxe-version " ^ (string_of_int gen.gcon.version) ^ " --feature-level 1" in
		print_endline cmd;
		if gen.gcon.run_command cmd <> 0 then failwith "Build failed";
		Sys.chdir old_dir;
	end

(* end of configure function *)

let before_generate con =
	let java_ver = try
			int_of_string (PMap.find "java_ver" con.defines)
		with | Not_found ->
			Common.define_value con Define.JavaVer "7";
			7
	in
 if java_ver < 5 then failwith ("Java version is defined to target Java " ^ string_of_int java_ver ^ ", but the compiler can only output code to versions equal or superior to Java 5");
	let rec loop i =
		Common.raw_define con ("java" ^ (string_of_int i));
		if i > 0 then loop (i - 1)
	in
	loop java_ver;
	()

let generate con =
	let exists = ref false in
	con.java_libs <- List.map (fun (file,std,close,la,gr) ->
		if String.ends_with file "hxjava-std.jar" then begin
			exists := true;
			(file,true,close,la,gr)
		end else
			(file,std,close,la,gr)) con.java_libs;
	if not !exists then
		failwith "Your version of hxjava is outdated. Please update it by running: `haxelib update hxjava`";
	let gen = new_ctx con in
	gen.gallow_tp_dynamic_conversion <- true;

	let basic = con.basic in
	(* make the basic functions in java *)
	let cl_cl = get_cl (get_type gen (["java";"lang"],"Class")) in
	let basic_fns =
	[
		mk_class_field "equals" (TFun(["obj",false,t_dynamic], basic.tbool)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "toString" (TFun([], basic.tstring)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "hashCode" (TFun([], basic.tint)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "getClass" (TFun([], (TInst(cl_cl,[t_dynamic])))) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "wait" (TFun([], basic.tvoid)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "notify" (TFun([], basic.tvoid)) true Ast.null_pos (Method MethNormal) [];
		mk_class_field "notifyAll" (TFun([], basic.tvoid)) true Ast.null_pos (Method MethNormal) [];
	] in
	List.iter (fun cf -> gen.gbase_class_fields <- PMap.add cf.cf_name cf gen.gbase_class_fields) basic_fns;

	(try
		configure gen
	with | TypeNotFound path -> con.error ("Error. Module '" ^ (s_type_path path) ^ "' is required and was not included in build.")	Ast.null_pos);
	debug_mode := false

(** Java lib *)

open JData

type java_lib_ctx = {
	jcom : Common.context;
	(* current tparams context *)
	mutable jtparams : jtypes list;
}

exception ConversionError of string * pos

let error s p = raise (ConversionError (s, p))

let is_haxe_keyword = function
	| "callback" | "cast" | "extern" | "function" | "in" | "typedef" | "using" | "var" | "untyped" | "inline" -> true
	| _ -> false

let jname_to_hx name =
	let name =
		if name <> "" && (String.get name 0 < 'A' || String.get name 0 > 'Z') then
			Char.escaped (Char.uppercase (String.get name 0)) ^ String.sub name 1 (String.length name - 1)
		else
			name
	in
	let name = String.concat "__" (String.nsplit name "_") in
	String.map (function | '$' -> '_' | c -> c) name

let normalize_pack pack =
	List.map (function
		| "" -> ""
		| str when String.get str 0 >= 'A' && String.get str 0 <= 'Z' ->
			String.lowercase str
		| str -> str
	) pack

let jpath_to_hx (pack,name) = match pack, name with
	| ["haxe";"root"], name -> [], name
	| "com" :: ("oracle" | "sun") :: _, _
	| "javax" :: _, _
	| "org" :: ("ietf" | "jcp" | "omg" | "w3c" | "xml") :: _, _
	| "sun" :: _, _
	| "sunw" :: _, _ -> "java" :: normalize_pack pack, jname_to_hx name
	| pack, name -> normalize_pack pack, jname_to_hx name

let real_java_path ctx (pack,name) =
	s_type_path (pack, name)

let lookup_jclass com path =
	let path = jpath_to_hx path in
	List.fold_right (fun (_,_,_,_,get_raw_class) acc ->
		match acc with
		| None -> get_raw_class path
		| Some p -> Some p
	) com.java_libs None

let mk_type_path ctx path params =
	let name, sub = try
		let p, _ = String.split (snd path) "$" in
		jname_to_hx p, Some (jname_to_hx (snd path))
		with | Invalid_string ->
			jname_to_hx (snd path), None
	in
	let pack = fst (jpath_to_hx path) in
	let pack, sub, name = match path with
		| [], ("Float" as c)
		| [], ("Int" as c)
		| [], ("Single" as c)
		| [], ("Bool" as c)
		| [], ("Dynamic" as c)
		| [], ("Iterator" as c)
		| [], ("ArrayAccess" as c)
		| [], ("Iterable" as c) ->
			[], Some c, "StdTypes"
		| [], ("String" as c) ->
			["std"], None, c
		| _ ->
			pack, sub, name
	in
	CTPath {
		tpackage = pack;
		tname = name;
		tparams = params;
		tsub = sub;
	}

let has_tparam name params = List.exists(fun (n,_,_) -> n = name) params

let rec convert_arg ctx p arg =
	match arg with
	| TAny | TType (WSuper, _) -> TPType (mk_type_path ctx ([], "Dynamic") [],null_pos)
	| TType (_, jsig) -> TPType (convert_signature ctx p jsig,null_pos)

and convert_signature ctx p jsig =
	match jsig with
	| TByte -> mk_type_path ctx (["java"; "types"], "Int8") []
	| TChar -> mk_type_path ctx (["java"; "types"], "Char16") []
	| TDouble -> mk_type_path ctx ([], "Float") []
	| TFloat -> mk_type_path ctx ([], "Single") []
	| TInt -> mk_type_path ctx ([], "Int") []
	| TLong -> mk_type_path ctx (["haxe"], "Int64") []
	| TShort -> mk_type_path ctx (["java"; "types"], "Int16") []
	| TBool -> mk_type_path ctx ([], "Bool") []
	| TObject ( (["haxe";"root"], name), args ) -> mk_type_path ctx ([], name) (List.map (convert_arg ctx p) args)
	(** nullable types *)
	(* replaced from Null<Type> to the actual abstract type to fix #2738 *)
	(* | TObject ( (["java";"lang"], "Integer"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Int") []) ] *)
	(* | TObject ( (["java";"lang"], "Double"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Float") []) ] *)
	(* | TObject ( (["java";"lang"], "Float"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Single") []) ] *)
	(* | TObject ( (["java";"lang"], "Boolean"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx ([], "Bool") []) ] *)
	(* | TObject ( (["java";"lang"], "Byte"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["java";"types"], "Int8") []) ] *)
	(* | TObject ( (["java";"lang"], "Character"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["java";"types"], "Char16") []) ] *)
	(* | TObject ( (["java";"lang"], "Short"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["java";"types"], "Int16") []) ] *)
	(* | TObject ( (["java";"lang"], "Long"), [] ) -> mk_type_path ctx ([], "Null") [ TPType (mk_type_path ctx (["haxe"], "Int64") []) ] *)
	(** other std types *)
	| TObject ( (["java";"lang"], "Object"), [] ) -> mk_type_path ctx ([], "Dynamic") []
	| TObject ( (["java";"lang"], "String"), [] ) -> mk_type_path ctx ([], "String") []
	| TObject ( (["java";"lang"], "Enum"), [_] ) -> mk_type_path ctx ([], "EnumValue") []
	(** other types *)
	| TObject ( path, [] ) ->
		(match lookup_jclass ctx.jcom path with
		| Some (jcl, _, _) -> mk_type_path ctx path (List.map (fun _ -> convert_arg ctx p TAny) jcl.ctypes)
		| None -> mk_type_path ctx path [])
	| TObject ( path, args ) -> mk_type_path ctx path (List.map (convert_arg ctx p) args)
	| TObjectInner (pack, (name, params) :: inners) ->
			let actual_param = match List.rev inners with
			| (_, p) :: _ -> p
			| _ -> assert false in
			mk_type_path ctx (pack, name ^ "$" ^ String.concat "$" (List.map fst inners)) (List.map (fun param -> convert_arg ctx p param) actual_param)
	| TObjectInner (pack, inners) -> assert false
	| TArray (jsig, _) -> mk_type_path ctx (["java"], "NativeArray") [ TPType (convert_signature ctx p jsig,null_pos) ]
	| TMethod _ -> JReader.error "TMethod cannot be converted directly into Complex Type"
	| TTypeParameter s -> (match ctx.jtparams with
		| cur :: others ->
			if has_tparam s cur then
				mk_type_path ctx ([], s) []
			else begin
				if ctx.jcom.verbose && not(List.exists (has_tparam s) others) then print_endline ("Type parameter " ^ s ^ " was not found while building type!");
				mk_type_path ctx ([], "Dynamic") []
			end
		| _ ->
			if ctx.jcom.verbose then print_endline ("Empty type parameter stack!");
			mk_type_path ctx ([], "Dynamic") [])

let convert_constant ctx p const =
	Option.map_default (function
		| ConstString s -> Some (EConst (String s), p)
		| ConstInt i -> Some (EConst (Int (Printf.sprintf "%ld" i)), p)
		| ConstFloat f | ConstDouble f -> Some (EConst (Float (Printf.sprintf "%E" f)), p)
		| _ -> None) None const

let rec same_sig parent jsig =
	match jsig with
	| TObject (p,targs) -> parent = p || List.exists (function | TType (_,s) -> same_sig parent s | _ -> false) targs
	| TObjectInner(p, ntargs) ->
			parent = (p, String.concat "$" (List.map fst ntargs)) ||
			List.exists (fun (_,targs) -> List.exists (function | TType(_,s) -> same_sig parent s | _ -> false) targs) ntargs
	| TArray(s,_) -> same_sig parent s
	| _ -> false

let convert_param ctx p parent param =
	let name, constraints = match param with
		| (name, Some extends_sig, implem_sig) ->
			name, extends_sig :: implem_sig
		| (name, None, implemem_sig) ->
			name, implemem_sig
		in
		let constraints = List.map (fun s -> if same_sig parent s then (TObject( (["java";"lang"], "Object"), [])) else s) constraints in
		{
			tp_name = name,null_pos;
			tp_params = [];
			tp_constraints = List.map (fun t -> convert_signature ctx p t,null_pos) constraints;
			tp_meta = [];
		}

let get_type_path ctx ct = match ct with | CTPath p -> p | _ -> assert false

let is_override field =
	List.exists (function | AttrVisibleAnnotations [{ ann_type = TObject( (["java";"lang"], "Override"), _ ) }] -> true | _ -> false) field.jf_attributes

let mk_override field =
	{ field with jf_attributes = ((AttrVisibleAnnotations [{ ann_type = TObject( (["java";"lang"], "Override"), [] ); ann_elements = [] }]) :: field.jf_attributes) }

let del_override field =
	{ field with jf_attributes = List.filter (fun a -> not (is_override_attrib a)) field.jf_attributes }

let get_canonical ctx p pack name =
	(Meta.JavaCanonical, [EConst (String (String.concat "." pack)), p; EConst (String name), p], p)

let convert_java_enum ctx p pe =
	let meta = ref (get_canonical ctx p (fst pe.cpath) (snd pe.cpath) :: [Meta.Native, [EConst (String (real_java_path ctx pe.cpath) ), p], p ]) in
	let data = ref [] in
	List.iter (fun f ->
		(* if List.mem JEnum f.jf_flags then *)
		match f.jf_vmsignature with
		| TObject( path, [] ) when path = pe.cpath && List.mem JStatic f.jf_flags && List.mem JFinal f.jf_flags ->
			data := { ec_name = f.jf_name,null_pos; ec_doc = None; ec_meta = []; ec_args = []; ec_pos = p; ec_params = []; ec_type = None; } :: !data;
		| _ -> ()
	) pe.cfields;

	EEnum {
		d_name = jname_to_hx (snd pe.cpath),null_pos;
		d_doc = None;
		d_params = []; (* enums never have type parameters *)
		d_meta = !meta;
		d_flags = [EExtern];
		d_data = List.rev !data;
	}

	let convert_java_field ctx p jc field =
		let p = { p with pfile =	p.pfile ^" (" ^field.jf_name ^")" } in
		let cff_doc = None in
		let cff_pos = p in
		let cff_meta = ref [] in
		let cff_access = ref [] in
		let cff_name = match field.jf_name with
			| "<init>" -> "new"
			| "<clinit>"-> raise Exit (* __init__ field *)
			| name when String.length name > 5 ->
					(match String.sub name 0 5 with
					| "__hx_" | "this$" -> raise Exit
					| _ -> name)
			| name -> name
		in
		let jf_constant = ref field.jf_constant in
		let readonly = ref false in

		List.iter (function
			| JPublic -> cff_access := APublic :: !cff_access
			| JPrivate -> raise Exit (* private instances aren't useful on externs *)
			| JProtected -> cff_access := APrivate :: !cff_access
			| JStatic -> cff_access := AStatic :: !cff_access
			| JFinal ->
				cff_meta := (Meta.Final, [], p) :: !cff_meta;
				(match field.jf_kind, field.jf_vmsignature, field.jf_constant with
				| JKField, TObject _, _ ->
					jf_constant := None
				| JKField, _, Some _ ->
					readonly := true;
					jf_constant := None;
				| _ -> jf_constant := None)
			(* | JSynchronized -> cff_meta := (Meta.Synchronized, [], p) :: !cff_meta *)
			| JVolatile -> cff_meta := (Meta.Volatile, [], p) :: !cff_meta
			| JTransient -> cff_meta := (Meta.Transient, [], p) :: !cff_meta
			(* | JVarArgs -> cff_meta := (Meta.VarArgs, [], p) :: !cff_meta *)
			| _ -> ()
		) field.jf_flags;

		List.iter (function
			| AttrDeprecated when jc.cpath <> (["java";"util"],"Date") -> cff_meta := (Meta.Deprecated, [], p) :: !cff_meta
			(* TODO: pass anotations as @:meta *)
			| AttrVisibleAnnotations ann ->
				List.iter (function
					| { ann_type = TObject( (["java";"lang"], "Override"), [] ) } ->
						cff_access := AOverride :: !cff_access
					| _ -> ()
				) ann
			| _ -> ()
		) field.jf_attributes;

		List.iter (fun jsig ->
			match convert_signature ctx p jsig with
				| CTPath path ->
					cff_meta := (Meta.Throws, [Ast.EConst (Ast.String (s_type_path (path.tpackage,path.tname))), p],p) :: !cff_meta
				| _ -> ()
		) field.jf_throws;

		let kind = match field.jf_kind with
			| JKField when !readonly ->
				FProp ("default", "null", Some (convert_signature ctx p field.jf_signature,null_pos), None)
			| JKField ->
				FVar (Some (convert_signature ctx p field.jf_signature,null_pos), None)
			| JKMethod ->
				match field.jf_signature with
				| TMethod (args, ret) ->
					let old_types = ctx.jtparams in
					(match ctx.jtparams with
					| c :: others -> ctx.jtparams <- (c @ field.jf_types) :: others
					| [] -> ctx.jtparams <- field.jf_types :: []);
					let i = ref 0 in
					let args = List.map (fun s ->
						incr i;
						("param" ^ string_of_int !i,null_pos), false, [], Some(convert_signature ctx p s,null_pos), None
					) args in
					let t = Option.map_default (convert_signature ctx p) (mk_type_path ctx ([], "Void") []) ret in
					cff_meta := (Meta.Overload, [], p) :: !cff_meta;

					let types = List.map (function
						| (name, Some ext, impl) ->
							{
								tp_name = name,null_pos;
								tp_params = [];
								tp_constraints = List.map (fun t -> convert_signature ctx p t,null_pos) (ext :: impl);
								tp_meta = [];
							}
						| (name, None, impl) ->
							{
								tp_name = name,null_pos;
								tp_params = [];
								tp_constraints = List.map (fun t -> convert_signature ctx p t,null_pos) (impl);
								tp_meta = [];
							}
					) field.jf_types in
					ctx.jtparams <- old_types;

					FFun ({
						f_params = types;
						f_args = args;
						f_type = Some (t,null_pos);
						f_expr = None
					})
				| _ -> error "Method signature was expected" p
		in
		let cff_name, cff_meta =
			match String.get cff_name 0 with
				| '%' ->
					let name = (String.sub cff_name 1 (String.length cff_name - 1)) in
					if not (is_haxe_keyword name) then
						cff_meta := (Meta.Deprecated, [EConst(String(
							"This static field `_" ^ name ^ "` is deprecated and will be removed in later versions. Please use `" ^ name ^ "` instead")
						),p], p) :: !cff_meta;
					"_" ^ name,
					(Meta.Native, [EConst (String (name) ), cff_pos], cff_pos) :: !cff_meta
				| _ ->
					match String.nsplit cff_name "$" with
						| [ no_dollar ] ->
							cff_name, !cff_meta
						| parts ->
							String.concat "_" parts,
							(Meta.Native, [EConst (String (cff_name) ), cff_pos], cff_pos) :: !cff_meta
		in
		if PMap.mem "java_loader_debug" ctx.jcom.defines then
			Printf.printf "\t%s%sfield %s : %s\n" (if List.mem AStatic !cff_access then "static " else "") (if List.mem AOverride !cff_access then "override " else "") cff_name (s_sig field.jf_signature);

		{
			cff_name = cff_name,null_pos;
			cff_doc = cff_doc;
			cff_pos = cff_pos;
			cff_meta = cff_meta;
			cff_access = !cff_access;
			cff_kind = kind
		}

	let rec japply_params params jsig = match params with
	| [] -> jsig
	| _ -> match jsig with
		| TTypeParameter s -> (try
			List.assoc s params
		with | Not_found -> jsig)
		| TObject(p,tl) ->
			TObject(p, args params tl)
		| TObjectInner(sl, stll) ->
			TObjectInner(sl, List.map (fun (s,tl) -> (s, args params tl)) stll)
		| TArray(s,io) ->
			TArray(japply_params params s, io)
		| TMethod(sl, sopt) ->
			TMethod(List.map (japply_params params) sl, Option.map (japply_params params) sopt)
		| _ -> jsig

	and args params tl = match params with
	| [] -> tl
	| _ -> List.map (function
		| TAny -> TAny
		| TType(w,s) -> TType(w,japply_params params s)) tl

	let mk_params jtypes = List.map (fun (s,_,_) -> (s,TTypeParameter s)) jtypes

	let convert_java_class ctx p jc =
		match List.mem JEnum jc.cflags with
		| true -> (* is enum *)
				[convert_java_enum ctx p jc]
		| false ->
			let flags = ref [HExtern] in
			if PMap.mem "java_loader_debug" ctx.jcom.defines then begin
				let sup = jc.csuper :: jc.cinterfaces in
				print_endline ("converting " ^ (if List.mem JAbstract jc.cflags then "abstract " else "") ^ JData.path_s jc.cpath ^ " : " ^ (String.concat ", " (List.map s_sig sup)));
			end;
			(* todo: instead of JavaNative, use more specific definitions *)
			let meta = ref [Meta.JavaNative, [], p; Meta.Native, [EConst (String (real_java_path ctx jc.cpath) ), p], p; get_canonical ctx p (fst jc.cpath) (snd jc.cpath)] in
			let force_check = Common.defined ctx.jcom Define.ForceLibCheck in
			if not force_check then
				meta := (Meta.LibType,[],p) :: !meta;

			let is_interface = ref false in
			List.iter (fun f -> match f with
				| JFinal -> meta := (Meta.Final, [], p) :: !meta
				| JInterface ->
						is_interface := true;
						flags := HInterface :: !flags
				| JAbstract -> meta := (Meta.Abstract, [], p) :: !meta
				| JAnnotation -> meta := (Meta.Annotation, [], p) :: !meta
				| _ -> ()
			) jc.cflags;

			(match jc.csuper with
				| TObject( (["java";"lang"], "Object"), _ ) -> ()
				| TObject( (["haxe";"lang"], "HxObject"), _ ) -> meta := (Meta.HxGen,[],p) :: !meta
				| _ -> flags := HExtends (get_type_path ctx (convert_signature ctx p jc.csuper),null_pos) :: !flags
			);

			List.iter (fun i ->
				match i with
				| TObject ( (["haxe";"lang"], "IHxObject"), _ ) -> meta := (Meta.HxGen,[],p) :: !meta
				| _ -> flags :=
					if !is_interface then
						HExtends (get_type_path ctx (convert_signature ctx p i),null_pos) :: !flags
					else
						HImplements (get_type_path ctx (convert_signature ctx p i),null_pos) :: !flags
			) jc.cinterfaces;

			let fields = ref [] in
			let jfields = ref [] in

			if jc.cpath <> (["java";"lang"], "CharSequence") then
				List.iter (fun f ->
					try
						if !is_interface && List.mem JStatic f.jf_flags then
							()
						else begin
							fields := convert_java_field ctx p jc f :: !fields;
							jfields := f :: !jfields
						end
					with
						| Exit -> ()
				) (jc.cfields @ jc.cmethods);

			(* make sure the throws types are imported correctly *)
			let imports = List.concat (List.map (fun f ->
				List.map (fun jsig ->
					match convert_signature ctx p jsig with
						| CTPath path ->
							let pos = { p with pfile = p.pfile ^ " (" ^ f.jf_name ^" @:throws)" } in
							EImport( List.map (fun s -> s,pos) (path.tpackage @ [path.tname]), INormal )
						| _ -> assert false
				) f.jf_throws
			) jc.cmethods) in

			(EClass {
				d_name = jname_to_hx (snd jc.cpath),null_pos;
				d_doc = None;
				d_params = List.map (convert_param ctx p jc.cpath) jc.ctypes;
				d_meta = !meta;
				d_flags = !flags;
				d_data = !fields;
			}) :: imports

	let create_ctx com =
		{
			jcom = com;
			jtparams = [];
		}

	let rec has_type_param = function
		| TTypeParameter _ -> true
		| TMethod (lst, opt) -> List.exists has_type_param lst || Option.map_default has_type_param false opt
		| TArray (s,_) -> has_type_param s
		| TObjectInner (_, stpl) -> List.exists (fun (_,sigs) -> List.exists has_type_param_arg sigs) stpl
		| TObject(_, pl) -> List.exists has_type_param_arg pl
		| _ -> false

	and has_type_param_arg = function | TType(_,s) -> has_type_param s | _ -> false

let rec japply_params jparams jsig = match jparams with
	| [] -> jsig
	| _ ->
		match jsig with
		| TObject(path,p) ->
			TObject(path, List.map (japply_params_tp jparams ) p)
		| TObjectInner(sl,stargl) ->
			TObjectInner(sl,List.map (fun (s,targ) -> (s, List.map (japply_params_tp jparams) targ)) stargl)
		| TArray(jsig,io) ->
			TArray(japply_params jparams jsig,io)
		| TMethod(args,ret) ->
			TMethod(List.map (japply_params jparams ) args, Option.map (japply_params jparams ) ret)
		| TTypeParameter s -> (try
			List.assoc s jparams
		with | Not_found -> jsig)
		| _ -> jsig


and japply_params_tp jparams jtype_argument = match jtype_argument with
	| TAny -> TAny
	| TType(w,jsig) -> TType(w,japply_params jparams jsig)

let mk_jparams jtypes params = match jtypes, params with
	| [], [] -> []
	| _, [] -> List.map (fun (s,_,_) -> s, TObject( (["java";"lang"], "Object"), [] ) ) jtypes
	| _ -> List.map2 (fun (s,_,_) jt -> match jt with
		| TAny -> s, TObject((["java";"lang"],"Object"),[])
		| TType(_,jsig) -> s, jsig) jtypes params

let rec compatible_signature_arg ?arg_test f1 f2 =
	let arg_test = match arg_test with
		| None -> (fun _ _ -> true)
		| Some a -> a
	in
	if f1 = f2 then
		true
	else match f1, f2 with
	| TObject(p,a), TObject(p2,a2) -> p = p2 && arg_test a a2
	| TObjectInner(sl, stal), TObjectInner(sl2, stal2) -> sl = sl2 && List.map fst stal = List.map fst stal2
	| TArray(s,_) , TArray(s2,_) -> compatible_signature_arg s s2
	| TTypeParameter t1 , TTypeParameter t2 -> t1 = t2
	| _ -> false

let rec compatible_param p1 p2 = match p1, p2 with
	| TType (_,s1), TType(_,s2) -> compatible_signature_arg ~arg_test:compatible_tparams s1 s2
	| TAny, TType(_, TObject( (["java";"lang"],"Object"), _ )) -> true
	| TType(_, TObject( (["java";"lang"],"Object"), _ )), TAny -> true
	| _ -> false

and compatible_tparams p1 p2 = try match p1, p2 with
	| [], [] -> true
	| _, [] ->
		let p2 = List.map (fun _ -> TAny) p1 in
		List.for_all2 compatible_param p1 p2
	| [], _ ->
		let p1 = List.map (fun _ -> TAny) p2 in
		List.for_all2 compatible_param p1 p2
	| _, _ ->
		List.for_all2 compatible_param p1 p2
	with | Invalid_argument("List.for_all2") -> false

let get_adapted_sig f f2 = match f.jf_types with
	| [] ->
		f.jf_signature
	| _ ->
		let jparams = mk_jparams f.jf_types (List.map (fun (s,_,_) -> TType(WNone, TTypeParameter s)) f2.jf_types) in
		japply_params jparams f.jf_signature

let compatible_methods f1 f2 =
	if List.length f1.jf_types <> List.length f2.jf_types then
		false
	else match (get_adapted_sig f1 f2), f2.jf_signature with
	| TMethod(a1,_), TMethod(a2,_) when List.length a1 = List.length a2 ->
		List.for_all2 compatible_signature_arg a1 a2
	| _ -> false

let jcl_from_jsig com jsig =
	let path, params = match jsig with
	| TObject(path, params) ->
		path,params
	| TObjectInner(sl, stll) ->
		let last_params = ref [] in
		let real_path = sl, String.concat "$" (List.map (fun (s,p) -> last_params := p; s) stll) in
		real_path, !last_params
	| _ -> raise Not_found
	in
	match lookup_jclass com path with
	| None -> raise Not_found
	| Some(c,_,_) -> c,params

let jclass_with_params com cls params = try
	match cls.ctypes with
	| [] -> cls
	| _ ->
		let jparams = mk_jparams cls.ctypes params in
		{ cls with
			cfields = List.map (fun f -> { f with jf_signature = japply_params jparams f.jf_signature }) cls.cfields;
			cmethods = List.map (fun f -> { f with jf_signature = japply_params jparams f.jf_signature }) cls.cmethods;
			csuper = japply_params jparams cls.csuper;
			cinterfaces = List.map (japply_params jparams) cls.cinterfaces;
		}
	with Invalid_argument("List.map2") ->
		if com.verbose then prerr_endline ("Differing parameters for class: " ^ s_type_path cls.cpath);
		cls

let is_object = function | TObject( (["java";"lang"], "Object"), [] ) -> true | _ -> false

let is_tobject = function | TObject _ | TObjectInner _ -> true | _ -> false

let simplify_args args =
	if List.for_all (function | TAny -> true | _ -> false) args then [] else args

let compare_type com s1 s2 =
	if s1 = s2 then
		0
	else if not (is_tobject s1) then
		if is_tobject s2 then (* Dynamic *)
			1
		else if compatible_signature_arg s1 s2 then
			0
		else
			raise Exit
	else if not (is_tobject s2) then
		-1
	else begin
		let rec loop ?(first_error=true) s1 s2 : bool =
			if is_object s1 then
				s1 = s2
			else if compatible_signature_arg s1 s2 then begin
				let p1, p2 = match s1, s2 with
				| TObject(_, p1), TObject(_,p2) ->
					p1, p2
				| TObjectInner(_, npl1), TObjectInner(_, npl2) ->
					snd (List.hd (List.rev npl1)), snd (List.hd (List.rev npl2))
				| _ -> assert false (* not tobject *)
				in
				let p1, p2 = simplify_args p1, simplify_args p2 in
				let lp1 = List.length p1 in
				let lp2 = List.length p2 in
				if lp1 > lp2 then
					true
				else if lp2 > lp1 then
					false
				else begin
					(* if compatible tparams, it's fine *)
					if not (compatible_tparams p1 p2) then
						raise Exit; (* meaning: found, but incompatible type parameters *)
					true
				end
			end else try
				let c, p = jcl_from_jsig com s1 in
				let jparams = mk_jparams c.ctypes p in
				let super = japply_params jparams c.csuper in
				let implements = List.map (japply_params jparams) c.cinterfaces in
				loop ~first_error:first_error super s2 || List.exists (fun super -> loop ~first_error:first_error super s2) implements
			with | Not_found ->
				prerr_endline ("-java-lib: The type " ^ (s_sig s1) ^ " is referred but was not found. Compilation may not occur correctly.");
				prerr_endline "Did you forget to include a needed lib?";
				if first_error then
					not (loop ~first_error:false s2 s1)
				else
					false
		in
		if loop s1 s2 then
			if loop s2 s1 then
				0
			else
				1
		else
			if loop s2 s1 then
				-1
			else
				-2
	end

(* given a list of same overload functions, choose the best (or none) *)
let select_best com flist =
	let rec loop cur_best = function
		| [] ->
			Some cur_best
		| f :: flist -> match get_adapted_sig f cur_best, cur_best.jf_signature with
			| TMethod(_,Some r), TMethod(_, Some r2) -> (try
				match compare_type com r r2 with
				| 0 -> (* same type - select any of them *)
					loop cur_best flist
				| 1 ->
					loop f flist
				| -1 ->
					loop cur_best flist
				| -2 -> (* error - no type is compatible *)
					if com.verbose then prerr_endline (f.jf_name ^ ": The types " ^ (s_sig r) ^ " and " ^ (s_sig r2) ^ " are incompatible");
					(* bet that the current best has "beaten" other types *)
					loop cur_best flist
				| _ -> assert false
			with | Exit -> (* incompatible type parameters *)
				(* error mode *)
				if com.verbose then prerr_endline (f.jf_name ^ ": Incompatible argument return signatures: " ^ (s_sig r) ^ " and " ^ (s_sig r2));
				None)
			| TMethod _, _ -> (* select the method *)
				loop f flist
			| _ ->
				loop cur_best flist
	in
	match loop (List.hd flist) (List.tl flist) with
	| Some f ->
		Some f
	| None -> match List.filter (fun f -> not (is_override f)) flist with
		(* error mode; take off all override methods *)
		| [] -> None
		| f :: [] -> Some f
		| f :: flist -> Some f (* pick one *)

(**** begin normalize_jclass helpers ****)

let fix_overrides_jclass com cls =
	let force_check = Common.defined com Define.ForceLibCheck in
	let methods = if force_check then List.map (fun f -> del_override f) cls.cmethods else cls.cmethods in
	let cmethods = methods in
	let super_fields = [] in
	let super_methods = [] in
	let nonstatics = List.filter (fun f -> not (List.mem JStatic f.jf_flags)) (cls.cfields @ cls.cmethods) in

	let is_pub = fun f -> List.exists (function | JPublic | JProtected -> true | _ -> false) f.jf_flags in
	let cmethods, super_fields = if not (List.mem JInterface cls.cflags) then
		List.filter is_pub cmethods,
		List.filter is_pub super_fields
	else
		cmethods,super_fields
	in

	let rec loop cls super_methods super_fields cmethods nonstatics = try
		match cls.csuper with
		| TObject((["java";"lang"],"Object"),_) ->
				super_methods,super_fields,cmethods,nonstatics
		| _ ->
			let cls, params = jcl_from_jsig com cls.csuper in
			let cls = jclass_with_params com cls params in
			let nonstatics = (List.filter (fun f -> (List.mem JStatic f.jf_flags)) (cls.cfields @ cls.cmethods)) @ nonstatics in
			let super_methods = cls.cmethods @ super_methods in
			let super_fields = cls.cfields @ super_fields in
			let cmethods = if force_check then begin
				let overriden = ref [] in
				let cmethods = List.map (fun jm ->
					(* TODO rewrite/standardize empty spaces *)
					if not (is_override jm) && not (List.mem JStatic jm.jf_flags) && List.exists (fun msup ->
						let ret = msup.jf_name = jm.jf_name && not(List.mem JStatic msup.jf_flags) && compatible_methods msup jm in
						if ret then begin
							let f = mk_override msup in
							overriden := { f with jf_flags = jm.jf_flags } :: !overriden
						end;
						ret
					) cls.cmethods then
						mk_override jm
					else
						jm
				) cmethods in
				!overriden @ cmethods
			end else
				cmethods
			in
			loop cls super_methods super_fields cmethods nonstatics
		with | Not_found ->
			super_methods,super_fields,cmethods,nonstatics
	in
	loop cls super_methods super_fields cmethods nonstatics

let normalize_jclass com cls =
	(* after adding the noCheck metadata, this option will annotate what changes were needed *)
	(* and that are now deprecated *)
	let force_check = Common.defined com Define.ForceLibCheck in
	(* fix overrides *)
	let super_methods, super_fields, cmethods, nonstatics = fix_overrides_jclass com cls in
	let all_methods = cmethods @ super_methods in

	(* look for interfaces and add missing implementations (may happen on abstracts or by vmsig differences *)
	(* (libType): even with libType enabled, we need to add these missing fields - otherwise we won't be able to use them from Haxe *)
	let added_interface_fields = ref [] in
	let rec loop_interface abstract cls iface = try
		match iface with
			| TObject ((["java";"lang"],"Object"), _) -> ()
			| TObject (path,_) when path = cls.cpath -> ()
			| _ ->
				let cif, params = jcl_from_jsig com iface in
				let cif = jclass_with_params com cif params in
				List.iter (fun jf ->
					if not(List.mem JStatic jf.jf_flags) && not (List.exists (fun jf2 -> jf.jf_name = jf2.jf_name && not (List.mem JStatic jf2.jf_flags) && jf.jf_signature = jf2.jf_signature) all_methods) then begin
						let jf = if abstract && force_check then del_override jf else jf in
						let jf = { jf with jf_flags = JPublic :: jf.jf_flags } in (* interfaces implementations are always public *)

						added_interface_fields := jf :: !added_interface_fields;
					end
				) cif.cmethods;
				(* we don't need to loop again in the interface unless we are in an abstract class, since these interfaces are already normalized *)
				if abstract then List.iter (loop_interface abstract cif) cif.cinterfaces;
		with Not_found -> ()
	in
	List.iter (loop_interface (List.mem JAbstract cls.cflags) cls) cls.cinterfaces;
	let nonstatics = !added_interface_fields @ nonstatics in
	let cmethods = !added_interface_fields @ cmethods in

	(* for each added field in the interface, lookup in super_methods possible methods to include *)
	(* so we can choose the better method still *)
	let cmethods = if not force_check then
		cmethods
	else
		List.fold_left (fun cmethods im ->
			(* see if any of the added_interface_fields need to be declared as override *)
			let f = List.find_all (fun jf -> jf.jf_name = im.jf_name && compatible_methods jf im) super_methods in
			let f = List.map mk_override f in
			f @ cmethods
		) cmethods !added_interface_fields;
	in

	(* take off equals, hashCode and toString from interface *)
	let cmethods = if List.mem JInterface cls.cflags then List.filter (fun jf -> match jf.jf_name, jf.jf_vmsignature with
			| "equals", TMethod([TObject( (["java";"lang"],"Object"), _)],_)
			| "hashCode", TMethod([], _)
			| "toString", TMethod([], _) -> false
			| _ -> true
	) cmethods
	else
		cmethods
	in

	(* change field name to not collide with haxe keywords and with static/non-static members *)
	let fold_field acc f =
		let change, both = match f.jf_name with
		| _ when List.mem JStatic f.jf_flags && List.exists (fun f2 -> f.jf_name = f2.jf_name) nonstatics -> true, true
		| _ -> is_haxe_keyword f.jf_name, false
		in
		let f2 = if change then
				{ f with jf_name = "%" ^ f.jf_name }
			else
				f
		in
		if both then f :: f2 :: acc else f2 :: acc
	in

	(* change static fields that have the same name as methods *)
	let cfields = List.fold_left fold_field [] cls.cfields in
	let cmethods = List.fold_left fold_field [] cmethods in
	(* take off variable fields that have the same name as methods *)
	(* and take off variables that already have been declared *)
	let filter_field f f2 = f != f2 && (List.mem JStatic f.jf_flags = List.mem JStatic f2.jf_flags) && f.jf_name = f2.jf_name && f2.jf_kind <> f.jf_kind in
	let cfields = List.filter (fun f ->
		if List.mem JStatic f.jf_flags then
			not (List.exists (filter_field f) cmethods)
		else
			not (List.exists (filter_field f) nonstatics) && not (List.exists (fun f2 -> f != f2 && f.jf_name = f2.jf_name && not (List.mem JStatic f2.jf_flags)) super_fields) ) cfields
	in
	(* now filter any method that clashes with a field - on a superclass *)
	let cmethods = if force_check then List.filter (fun f ->
		if List.mem JStatic f.jf_flags then
			true
		else
			not (List.exists (filter_field f) super_fields) ) cmethods
	else
		cmethods
	in
	(* removing duplicate fields. They are there because of return type covariance in Java *)
	(* Also, if a method overrides a previous definition, and changes a type parameters' variance, *)
	(* we will take it off *)
	(* this means that some rare codes will never compile on Haxe, but unless Haxe adds variance support *)
	(* I can't see how this can be any different *)
	let rec loop acc = function
		| [] -> acc
		| f :: cmeths ->
			match List.partition (fun f2 -> f.jf_name = f2.jf_name && compatible_methods f f2) cmeths with
			| [], cmeths ->
				loop (f :: acc) cmeths
			| flist, cmeths -> match select_best com (f :: flist) with
				| None ->
					loop acc cmeths
				| Some f ->
					loop (f :: acc) cmeths
	in
	(* last pass: take off all cfields that are internal / private (they won't be accessible anyway) *)
	let cfields = List.filter(fun f -> List.exists (fun f -> f = JPublic || f = JProtected) f.jf_flags) cfields in
	let cmethods = loop [] cmethods in
	{ cls with cfields = cfields; cmethods = cmethods }

(**** end normalize_jclass helpers ****)

let get_classes_zip zip =
	let ret = ref [] in
	List.iter (function
		| { Zip.is_directory = false; Zip.filename = f } when (String.sub (String.uncapitalize f) (String.length f - 6) 6) = ".class" && not (String.exists f "$") ->
				(match List.rev (String.nsplit f "/") with
				| clsname :: pack ->
					if not (String.contains clsname '$') then begin
						let path = jpath_to_hx (List.rev pack, String.sub clsname 0 (String.length clsname - 6)) in
						ret := path :: !ret
					end
				| _ ->
						ret := ([], jname_to_hx f) :: !ret)
		| _ -> ()
	) (Zip.entries zip);
	!ret

let add_java_lib com file std =
	let file = if Sys.file_exists file then
		file
	else try Common.find_file com file with
		| Not_found -> try Common.find_file com (file ^ ".jar") with
		| Not_found ->
			failwith ("Java lib " ^ file ^ " not found")
	in
	let hxpack_to_jpack = Hashtbl.create 16 in
	let get_raw_class, close, list_all_files =
		(* check if it is a directory or jar file *)
		match (Unix.stat file).st_kind with
		| S_DIR -> (* open classes directly from directory *)
			let all = ref [] in
			let rec iter_files pack dir path = try
				let file = Unix.readdir dir in
				let filepath = path ^ "/" ^ file in
				(if String.ends_with file ".class" && not (String.exists file "$") then
					let file = String.sub file 0 (String.length file - 6) in
					let path = jpath_to_hx (pack,file) in
					all := path :: !all;
					Hashtbl.add hxpack_to_jpack path (pack,file)
				else if (Unix.stat filepath).st_kind = S_DIR && file <> "." && file <> ".." then
					let pack = pack @ [file] in
					iter_files (pack) (Unix.opendir filepath) filepath);
				iter_files pack dir path
			with | End_of_file | Unix.Unix_error _ ->
				Unix.closedir dir
			in
			iter_files [] (Unix.opendir file) file;
			let all = !all in

			(fun (pack, name) ->
				let real_path = file ^ "/" ^ (String.concat "/" pack) ^ "/" ^ (name ^ ".class") in
				try
					let data = Std.input_file ~bin:true real_path in
					Some(JReader.parse_class (IO.input_string data), real_path, real_path)
				with
					| _ -> None), (fun () -> ()), (fun () -> all)
		| _ -> (* open zip file *)
			let closed = ref false in
			let zip = ref (Zip.open_in file) in
			let check_open () =
				if !closed then begin
					prerr_endline ("JAR file " ^ file ^ " already closed"); (* if this happens, find when *)
					zip := Zip.open_in file;
					closed := false
				end
			in
			List.iter (function
				| { Zip.is_directory = false; Zip.filename = filename } when String.ends_with filename ".class" ->
					let pack = String.nsplit filename "/" in
					(match List.rev pack with
						| [] -> ()
						| name :: pack ->
							let name = String.sub name 0 (String.length name - 6) in
							let pack = List.rev pack in
							Hashtbl.add hxpack_to_jpack (jpath_to_hx (pack,name)) (pack,name))
				| _ -> ()
			) (Zip.entries !zip);
			(fun (pack, name) ->
				check_open();
				try
					let location = (String.concat "/" (pack @ [name]) ^ ".class") in
					let entry = Zip.find_entry !zip location in
					let data = Zip.read_entry !zip entry in
					Some(JReader.parse_class (IO.input_string data), file, file ^ "@" ^ location)
				with
					| Not_found ->
						None),
			(fun () -> if not !closed then begin closed := true; Zip.close_in !zip end),
			(fun () -> check_open(); get_classes_zip !zip)
	in
	let cached_types = Hashtbl.create 12 in
	let get_raw_class path =
		try
			Hashtbl.find cached_types path
		with | Not_found -> try
			let pack, name = Hashtbl.find hxpack_to_jpack path in
			let try_file (pack,name) =
				match get_raw_class (pack,name) with
				| None ->
						Hashtbl.add cached_types path None;
						None
				| Some (i, p1, p2) ->
						Hashtbl.add cached_types path (Some(i,p1,p2)); (* type loop normalization *)
						let ret = Some (normalize_jclass com i, p1, p2) in
						Hashtbl.replace cached_types path ret;
						ret
			in
			try_file (pack,name)
		with Not_found ->
			None
	in
	let replace_canonical_name p pack name_original name_replace decl =
		let mk_meta name = (Meta.JavaCanonical, [EConst (String (String.concat "." pack)), p; EConst(String name), p], p) in
		let add_meta name metas =
			if Meta.has Meta.JavaCanonical metas then
				List.map (function
					| (Meta.JavaCanonical,[EConst (String cpack), _; EConst(String cname), _],_) ->
						let did_replace,name = String.replace cname name_original name_replace in
						if not did_replace then print_endline (cname ^ " -> " ^ name_original ^ " -> " ^ name_replace);
						mk_meta name
					| m -> m
				) metas
			else
				mk_meta name :: metas
		in
		match decl with
			| EClass c ->
				EClass { c with d_meta = add_meta (fst c.d_name) c.d_meta }
			| EEnum e ->
				EEnum { e with d_meta = add_meta (fst e.d_name) e.d_meta }
			| EAbstract a ->
				EAbstract { a with d_meta = add_meta (fst a.d_name) a.d_meta }
			| d -> d
	in
	let rec build ctx path p types =
		try
			if List.mem path !types then
				None
			else begin
				let first = match !types with
					| [ ["java";"lang"], "String" ] | [] -> true
					| p :: _ ->
						false
				in
				types := path :: !types;
				match get_raw_class path, path with
				| None, ([], c) -> build ctx (["haxe";"root"], c) p types
				| None, _ -> None
				| Some (cls, real_path, pos_path), _ ->
						let is_disallowed_inner = first && String.exists (snd cls.cpath) "$" in
						let is_disallowed_inner = if is_disallowed_inner then begin
								let outer, inner = String.split (snd cls.cpath) "$" in
								match get_raw_class (fst path, outer) with
									| None -> false
									| _ -> true
							end else
								false
						in
						if is_disallowed_inner then
							None
						else begin
							if com.verbose then print_endline ("Parsed Java class " ^ (s_type_path cls.cpath));
							let old_types = ctx.jtparams in
							ctx.jtparams <- cls.ctypes :: ctx.jtparams;

							let pos = { pfile = pos_path; pmin = 0; pmax = 0; } in

							let pack = match fst path with | ["haxe";"root"] -> [] | p -> p in

							let ppath = Hashtbl.find hxpack_to_jpack path in
							let inner = List.fold_left (fun acc (path,out,_,_) ->
								let path = jpath_to_hx path in
								(if out <> Some ppath then
									acc
								else match build ctx path p types with
									| Some(_,(_, classes)) ->
										let base = snd ppath ^ "$" in
										(List.map (fun (def,p) ->
											replace_canonical_name p (fst ppath) base (snd ppath ^ ".") def, p) classes) @ acc
									| _ -> acc);
							) [] cls.cinner_types in

							(* add _Statics class *)
							let inner = try
								if not (List.mem JInterface cls.cflags) then raise Not_found;
								let smethods = List.filter (fun f -> List.mem JStatic f.jf_flags) cls.cmethods in
								let sfields = List.filter (fun f -> List.mem JStatic f.jf_flags) cls.cfields in
								if not (smethods <> [] || sfields <> []) then raise Not_found;
								let obj = TObject( (["java";"lang"],"Object"), []) in
								let ncls = convert_java_class ctx pos { cls with cmethods = smethods; cfields = sfields; cflags = []; csuper = obj; cinterfaces = []; cinner_types = []; ctypes = [] } in
								match ncls with
								| EClass c :: imports ->
									(EClass { c with d_name = (fst c.d_name ^ "_Statics"),snd c.d_name }, pos) :: inner @ List.map (fun i -> i,pos) imports
								| _ -> assert false
							with | Not_found ->
								inner
							in
							let inner_alias = ref SS.empty in
							List.iter (fun x ->
								match fst x with
								| EClass c ->
									inner_alias := SS.add (fst c.d_name) !inner_alias;
								| _ -> ()
							) inner;
							let alias_list = ref [] in
							List.iter (fun x ->
								match x with
								| (EClass c, pos) -> begin
									let parts = String.nsplit (fst c.d_name) "_24" in
									match parts with
										| _ :: _ ->
											let alias_name = String.concat "_" parts in
											if (not (SS.mem alias_name !inner_alias)) && (not (String.exists (snd path) "_24")) then begin
												let alias_def = ETypedef {
													d_name = alias_name,null_pos;
													d_doc = None;
													d_params = c.d_params;
													d_meta = [];
													d_flags = [];
													d_data = CTPath {
														tpackage = pack;
														tname = snd path;
														tparams = List.map (fun tp ->
															TPType (CTPath {
																tpackage = [];
																tname = fst tp.tp_name;
																tparams = [];
																tsub = None;
															},null_pos)
														) c.d_params;
														tsub = Some(fst c.d_name);
													},null_pos;
												} in
												inner_alias := SS.add alias_name !inner_alias;
												alias_list := (alias_def, pos) :: !alias_list;
											end
										| _ -> ()
								end
								| _ -> ()
							) inner;
							let inner = List.concat [!alias_list ; inner] in
							let classes = List.map (fun t -> t,pos) (convert_java_class ctx pos cls) in
							let imports, defs = List.partition (function | (EImport(_),_) -> true | _ -> false) (classes @ inner) in
							let ret = Some ( real_path, (pack, imports @ defs) ) in
							ctx.jtparams <- old_types;
							ret
						end
			end
		with
		| JReader.Error_message msg ->
			prerr_endline ("Class reader failed: " ^ msg);
			None
		| e ->
			if com.verbose then begin
				(* prerr_endline (Printexc.get_backtrace ()); requires ocaml 3.11 *)
				prerr_endline (Printexc.to_string e)
			end;
			None
	in
	let build path p = build (create_ctx com) path p (ref [["java";"lang"], "String"]) in
	let cached_files = ref None in
	let list_all_files () = match !cached_files with
		| None ->
				let ret = list_all_files () in
				cached_files := Some ret;
				ret
		| Some r -> r
	in

	(* TODO: add_dependency m mdep *)
	com.load_extern_type <- com.load_extern_type @ [build];
	com.java_libs <- (file, std, close, list_all_files, get_raw_class) :: com.java_libs
