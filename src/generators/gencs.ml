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

open ReflectionCFs
open Globals
open Ast
open Common
open Type
open Gencommon
open Gencommon.SourceWriter
open Codegen
open Printf
open Option
open ExtString

let rec is_cs_basic_type t =
	match follow t with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TInst( { cl_path = (["haxe"], "Int64") }, [] )
		| TAbstract ({ a_path = (["cs"], "Int64") },[])
		| TAbstract ({ a_path = (["cs"], "UInt64") },[])
		| TAbstract ({ a_path = ([], "Int") },[])
		| TAbstract ({ a_path = ([], "Float") },[])
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| TAbstract ({ a_path = (["cs"], "Pointer") },_) ->
			false
		| TAbstract _ when like_float t ->
			true
		| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
			is_cs_basic_type (Abstract.get_underlying_type a pl)
		| TEnum(e, _) when not (Meta.has Meta.Class e.e_meta) -> true
		| TInst(cl, _) when Meta.has Meta.Struct cl.cl_meta -> true
		| _ -> false

let binops_names = List.fold_left (fun acc (op,n) -> PMap.add n op acc) PMap.empty Dotnet.cs_binops
let unops_names = List.fold_left (fun acc (op,n) -> PMap.add n op acc) PMap.empty Dotnet.cs_unops

let is_tparam t =
	match follow t with
		| TInst( { cl_kind = KTypeParameter _ }, [] ) -> true
		| _ -> false

let rec is_int_float gen t =
	match follow (gen.greal_type t) with
		| TInst( { cl_path = (["haxe"], "Int32") }, [] )
		| TAbstract ({ a_path = ([], "Int") },[])
		| TAbstract ({ a_path = ([], "Float") },[]) ->
			true
		| TAbstract _ when like_float t && not (like_i64 t) ->
			true
		| TInst( { cl_path = (["haxe"; "lang"], "Null") }, [t] ) -> is_int_float gen t
		| _ -> false

let is_bool t =
	match follow t with
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| _ -> false

let is_exactly_bool gen t =
	match gen.gfollow#run_f t with
		| TAbstract ({ a_path = ([], "Bool") },[]) ->
			true
		| _ -> false

let is_dynamic gen t =
	match follow (gen.greal_type t) with
		| TDynamic _ -> true
		| _ -> false

let is_pointer gen t =
	match follow (gen.greal_type t) with
		| TAbstract( ( {a_path = ["cs"], "Pointer"}, _ ) )
		| TInst( {cl_path = ["cs"], "Pointer"}, _ ) -> true
		| _ -> false

let rec is_null t =
	match t with
		| TInst( { cl_path = (["haxe"; "lang"], "Null") }, _ )
		| TAbstract( { a_path = ([], "Null") }, _ ) -> true
		| TType( t, tl ) -> is_null (apply_params t.t_params tl t.t_type)
		| TMono r ->
			(match !r with
			| Some t -> is_null t
			| _ -> false)
		| TLazy f ->
			is_null (lazy_type f)
		| _ -> false

let rec get_ptr e = match e.eexpr with
	| TParenthesis e | TMeta(_,e)
	| TCast(e,_) -> get_ptr e
	| TCall( { eexpr = TIdent "__ptr__" }, [ e ] ) ->
		Some e
	| _ -> None

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

let rec change_md = function
	| TAbstractDecl(a) when Meta.has Meta.Delegate a.a_meta && not (Meta.has Meta.CoreType a.a_meta) ->
		change_md (t_to_md a.a_this)
	| TClassDecl( { cl_kind = KAbstractImpl ({ a_this = TInst(impl,_) } as a) }) when Meta.has Meta.Delegate a.a_meta ->
		TClassDecl impl
	| TClassDecl( { cl_kind = KAbstractImpl (a) }) when Meta.has Meta.CoreType a.a_meta ->
		TAbstractDecl a
	| md -> md

(* used in c#-specific filters to skip some of them for the special haxe.lang.Runtime class *)
let in_runtime_class gen =
	match gen.gcurrent_class with
	| Some { cl_path = ["haxe";"lang"],"Runtime"} -> true
	| _ -> false

(* ******************************************* *)
(* CSharpSpecificESynf *)
(* ******************************************* *)
(*
	Some CSharp-specific syntax filters that must run before ExpressionUnwrap

	dependencies:
		It must run before ExprUnwrap, as it may not return valid Expr/Statement expressions
		It must run before ClassInstance, as it will detect expressions that need unchanged TTypeExpr
*)
module CSharpSpecificESynf =
struct
	let name = "csharp_specific_e"
	let priority = solve_deps name [DBefore ExpressionUnwrap.priority; DBefore ClassInstance.priority]

	let get_cl_from_t t =
		match follow t with
			| TInst(cl,_) -> cl
			| _ -> assert false

	let get_ab_from_t t =
		match follow t with
			| TAbstract(ab,_) -> ab
			| _ -> assert false

	let configure gen runtime_cl =
		let basic = gen.gcon.basic in
		let uint = match get_type gen ([], "UInt") with | TTypeDecl t -> TType(t, []) | TAbstractDecl a -> TAbstract(a, []) | _ -> assert false in

		let rec run e =
			match e.eexpr with
				(* Std.is() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "is" })) },
						[ obj; { eexpr = TTypeExpr(TClassDecl { cl_path = [], "Dynamic" } | TAbstractDecl { a_path = [], "Dynamic" }) }]
					) ->
						Type.map_expr run e
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "is"}) ) },
						[ obj; { eexpr = TTypeExpr(md) }]
					) ->
					let md = change_md md in
					let mk_is obj md =
						{ e with eexpr = TCall( { eexpr = TIdent "__is__"; etype = t_dynamic; epos = e.epos }, [
							obj;
							{ eexpr = TTypeExpr md; etype = t_dynamic (* this is after all a syntax filter *); epos = e.epos }
						] ) }
					in

					let mk_or a b =
						{
							eexpr = TBinop(Ast.OpBoolOr, a, b);
							etype = basic.tbool;
							epos = e.epos
						}
					in

					let wrap_if_needed obj f =
						(* introduce temp variable for complex expressions *)
						match obj.eexpr with
							| TLocal(v) -> f obj
							| _ ->
								let var = mk_temp "is" obj.etype in
								let added = { obj with eexpr = TVar(var, Some(obj)); etype = basic.tvoid } in
								let local = mk_local var obj.epos in
								{
									eexpr = TBlock([ added; f local ]);
									etype = basic.tbool;
									epos = e.epos
								}
					in

					let obj = run obj in
					(match follow_module follow md with
						| TAbstractDecl{ a_path = ([], "Float") } when not (in_runtime_class gen) ->
							(* on the special case of seeing if it is a Float, we need to test if both it is a float and if it is an Int *)
							let mk_is local =
								(* we check if it float or int or uint *)
								let eisint = mk_is local (TAbstractDecl (get_ab_from_t basic.tint)) in
								let eisuint = mk_is local (TAbstractDecl (get_ab_from_t uint)) in
								let eisfloat = mk_is local md in
								mk_paren (mk_or eisfloat (mk_or eisint eisuint))
							in
							wrap_if_needed obj mk_is

						| TAbstractDecl{ a_path = ([], "Int") } when not (in_runtime_class gen) ->
							(* int can be stored in double variable because of anonymous functions, check that case *)
							let mk_isint_call local =
								{
									eexpr = TCall(
										mk_static_field_access_infer runtime_cl "isInt" e.epos [],
										[ local ]
									);
									etype = basic.tbool;
									epos = e.epos
								}
							in
							let mk_is local =
								let eisint = mk_is local (TAbstractDecl (get_ab_from_t basic.tint)) in
								let eisuint = mk_is local (TAbstractDecl (get_ab_from_t uint)) in
								mk_paren (mk_or (mk_or eisint eisuint) (mk_isint_call local))
							in
							wrap_if_needed obj mk_is

						| TAbstractDecl{ a_path = ([], "UInt") } when not (in_runtime_class gen) ->
							(* uint can be stored in double variable because of anonymous functions, check that case *)
							let mk_isuint_call local =
								{
									eexpr = TCall(
										mk_static_field_access_infer runtime_cl "isUInt" e.epos [],
										[ local ]
									);
									etype = basic.tbool;
									epos = e.epos
								}
							in
							let mk_is local =
								let eisuint = mk_is local (TAbstractDecl (get_ab_from_t uint)) in
								mk_paren (mk_or eisuint (mk_isuint_call local))
							in
							wrap_if_needed obj mk_is

						| _ ->
							mk_is obj md
					)
				(* end Std.is() *)

				| TBinop( Ast.OpUShr, e1, e2 ) ->
					mk_cast e.etype { e with eexpr = TBinop( Ast.OpShr, mk_cast uint (run e1), run e2 ) }

				| TBinop( Ast.OpAssignOp Ast.OpUShr, e1, e2 ) ->
					let mk_ushr local =
						{ e with eexpr = TBinop(Ast.OpAssign, local, run { e with eexpr = TBinop(Ast.OpUShr, local, run e2) }) }
					in

					let mk_local obj =
						let var = mk_temp "opUshr" obj.etype in
						let added = { obj with eexpr = TVar(var, Some(obj)); etype = basic.tvoid } in
						let local = mk_local var obj.epos in
						local, added
					in

					let e1 = run e1 in

					let ret = match e1.eexpr with
						| TField({ eexpr = TLocal _ }, _)
						| TField({ eexpr = TTypeExpr _ }, _)
						| TArray({ eexpr = TLocal _ }, _)
						| TLocal(_) ->
							mk_ushr e1
						| TField(fexpr, field) ->
							let local, added = mk_local fexpr in
							{ e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TField(local, field) }  ]); }
						| TArray(ea1, ea2) ->
							let local, added = mk_local ea1 in
							{ e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TArray(local, ea2) }  ]); }
						| _ -> (* invalid left-side expression *)
							assert false
					in

					ret

				| _ -> Type.map_expr run e
		in
		gen.gsyntax_filters#add name (PCustom priority) run
end;;

(* ******************************************* *)
(* CSharpSpecificSynf *)
(* ******************************************* *)

(*

	Some CSharp-specific syntax filters  that can run after ExprUnwrap

	dependencies:
		Runs after ExprUnwrap

*)

module CSharpSpecificSynf =
struct
	let name = "csharp_specific"
	let priority = solve_deps name [ DAfter ExpressionUnwrap.priority; DAfter ObjectDeclMap.priority; DAfter ArrayDeclSynf.priority;	DAfter HardNullableSynf.priority ]

	let get_cl_from_t t =
		match follow t with
			| TInst(cl,_) -> cl
			| _ -> assert false

	let is_tparam t =
		match follow t with
			| TInst( { cl_kind = KTypeParameter _ }, _ ) -> true
			| _ -> false

	let configure gen runtime_cl =
		let basic = gen.gcon.basic in
		let tchar = match ( get_type gen (["cs"], "Char16") ) with
			| TTypeDecl t -> TType(t,[])
			| TAbstractDecl a -> TAbstract(a,[])
			| _ -> assert false
		in
		let string_ext = get_cl ( get_type gen (["haxe";"lang"], "StringExt")) in
		let ti64 = match ( get_type gen (["cs"], "Int64") ) with | TTypeDecl t -> TType(t,[]) | TAbstractDecl a -> TAbstract(a,[]) | _ -> assert false in
		let boxed_ptr =
			if Common.defined gen.gcon Define.Unsafe then
				get_cl (get_type gen (["haxe";"lang"], "BoxedPointer"))
				(* get_abstract (get_type gen (["cs"],"Pointer")) *)
			else
				null_class
		in

		let is_struct t = (* not basic type *)
			match follow t with
				| TInst(cl, _) when Meta.has Meta.Struct cl.cl_meta -> true
				| _ -> false
		in

		let is_cl t = match gen.greal_type t with | TInst ( { cl_path = (["System"], "Type") }, [] ) -> true | _ -> false in

		let fast_cast = Common.defined gen.gcon Define.FastCast in

		let rec run e =
			match e.eexpr with

				(* Std.int() *)
				| TCall(
						{ eexpr = TField( _, FStatic({ cl_path = ([], "Std") }, { cf_name = "int" }) ) },
						[obj]
					) ->
					run (mk_cast basic.tint obj)
				(* end Std.int() *)

				(* TODO: change cf_name *)
				| TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = "length" })) ->
					{ e with eexpr = TField(run ef, FDynamic "Length") }
				| TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = "toLowerCase" })) ->
					{ e with eexpr = TField(run ef, FDynamic "ToLowerInvariant") }
				| TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = "toUpperCase" })) ->
					{ e with eexpr = TField(run ef, FDynamic "ToUpperInvariant") }

				| TCall( { eexpr = TField(_, FStatic({ cl_path = [], "String" }, { cf_name = "fromCharCode" })) }, [cc] ) ->
					{ e with eexpr = TNew(get_cl_from_t basic.tstring, [], [mk_cast tchar (run cc); ExprBuilder.make_int gen.gcon 1 cc.epos]) }
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("charAt" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("charCodeAt" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("indexOf" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("lastIndexOf" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("split" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("substring" as field) })) }, args )
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("substr" as field) })) }, args ) ->
					{ e with eexpr = TCall(mk_static_field_access_infer string_ext field e.epos [], [run ef] @ (List.map run args)) }
				| TCall( { eexpr = TField(ef, FInstance({ cl_path = [], "String" }, _, { cf_name = ("toString") })) }, [] ) ->
					run ef
				| TNew( { cl_path = ([], "String") }, [], [p] ) -> run p (* new String(myString) -> myString *)

				| TCast(expr, _) when like_float expr.etype && is_pointer gen e.etype ->
					let expr = run expr in
					mk_cast e.etype (mk_cast ti64 expr)
				| TCast(expr, _) when is_dynamic gen expr.etype && is_pointer gen e.etype ->
					(match get_ptr expr with
						| None ->
							(* unboxing *)
							let expr = run expr in
							mk_cast e.etype (mk_field_access gen (mk_cast (TInst(boxed_ptr,[])) expr) "value" e.epos)
						| Some e ->
							run e)
				| TCast(expr, _) when is_pointer gen expr.etype && is_dynamic gen e.etype ->
					(match get_ptr expr with
						| None ->
							(* boxing *)
							let expr = run expr in
							{ e with eexpr = TNew(boxed_ptr,[],[expr]) }
						| Some e ->
							run e)
				| TCast(expr, _) when is_bool e.etype && is_dynamic gen expr.etype ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toBool" expr.epos [],
							[ run expr ]
						);
						etype = basic.tbool;
						epos = e.epos
					}
				| TCast(expr, _) when is_int_float gen e.etype && is_dynamic gen expr.etype && ( Common.defined gen.gcon Define.EraseGenerics || not (is_null e.etype) ) && not (in_runtime_class gen) ->
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
						etype = basic.tint;
						epos = expr.epos
					} in

					if needs_cast then mk_cast e.etype ret else ret

				| TCast(expr, _) when Common.defined gen.gcon Define.EraseGenerics && like_i64 e.etype && is_dynamic gen expr.etype && not (in_runtime_class gen) ->
					{
						eexpr = TCall(
							mk_static_field_access_infer runtime_cl "toLong" expr.epos [],
							[ run expr ]
						);
						etype = ti64;
						epos = expr.epos
					}

				| TCast(expr, Some(TClassDecl cls)) when fast_cast && cls == null_class ->
					if is_cs_basic_type (gen.greal_type e.etype) || is_tparam (gen.greal_type e.etype) then
						{ e with eexpr = TCast(run expr, Some(TClassDecl null_class)) }
					else
						{ e with eexpr = TCall(mk (TIdent "__as__") t_dynamic e.epos, [run expr]) }

				| TCast(expr, _) when (is_string e.etype) && (not (is_string expr.etype)) && not (in_runtime_class gen) ->
					{ e with eexpr = TCall( mk_static_field_access_infer runtime_cl "toString" expr.epos [], [run expr] ) }

				| TCast(expr, _) when is_tparam e.etype && not (in_runtime_class gen) && not (Common.defined gen.gcon Define.EraseGenerics) ->
					let static = mk_static_field_access_infer (runtime_cl) "genericCast" e.epos [e.etype] in
					{ e with eexpr = TCall(static, [mk (TIdent "$type_param") e.etype expr.epos; run expr]); }

				| TBinop( (Ast.OpNotEq as op), e1, e2)
				| TBinop( (Ast.OpEq as op), e1, e2) when is_struct e1.etype || is_struct e2.etype ->
					let mk_ret e = match op with | Ast.OpNotEq -> { e with eexpr = TUnop(Ast.Not, Ast.Prefix, e) } | _ -> e in
					mk_ret { e with
						eexpr = TCall({
							eexpr = TField(run e1, FDynamic "Equals");
							etype = TFun(["obj1",false,t_dynamic;], basic.tbool);
							epos = e1.epos
						}, [ run e2 ])
					}

				| TBinop ( (Ast.OpEq as op), e1, e2 )
				| TBinop ( (Ast.OpNotEq as op), e1, e2 ) when is_cl e1.etype && not (in_runtime_class gen) ->
					let static = mk_static_field_access_infer (runtime_cl) "typeEq" e.epos [] in
					let ret = { e with eexpr = TCall(static, [run e1; run e2]); } in
					if op = Ast.OpNotEq then
						{ ret with eexpr = TUnop(Ast.Not, Ast.Prefix, ret) }
					else
						ret

				| _ -> Type.map_expr run e
		in
		gen.gsyntax_filters#add name (PCustom priority) run
end;;

let add_cast_handler gen =
	let basic = gen.gcon.basic in
	(*
		starting to set gtparam_cast.
	*)

	(* NativeArray: the most important. *)

	(*
		var new_arr = new NativeArray<TO_T>(old_arr.Length);
		var i = -1;
		while( i < old_arr.Length )
		{
			new_arr[i] = (TO_T) old_arr[i];
		}
	*)

	let native_arr_cl = get_cl ( get_type gen (["cs"], "NativeArray") ) in

	let get_narr_param t = match follow t with
		| TInst({ cl_path = (["cs"], "NativeArray") }, [param]) -> param
		| _ -> assert false
	in

	let gtparam_cast_native_array e to_t =
		let old_param = get_narr_param e.etype in
		let new_param = get_narr_param to_t in

		let new_v = mk_temp "new_arr" to_t in
		let i = mk_temp "i" basic.tint in
		let old_len = mk_field_access gen e "Length" e.epos in
		let obj_v = mk_temp "obj" t_dynamic in
		let check_null = {eexpr = TBinop(Ast.OpNotEq, e, null e.etype e.epos); etype = basic.tbool; epos = e.epos} in
		let block = [
			{
				eexpr = TVar(
					new_v, Some( {
						eexpr = TNew(native_arr_cl, [new_param], [old_len] );
						etype = to_t;
						epos = e.epos
					} )
				);
				etype = basic.tvoid;
				epos = e.epos
			};
			{
				eexpr = TVar(i, Some( ExprBuilder.make_int gen.gcon (-1) e.epos ));
				etype = basic.tvoid;
				epos = e.epos
			};
			{
				eexpr = TWhile(
					{
						eexpr = TBinop(
							Ast.OpLt,
							{ eexpr = TUnop(Ast.Increment, Ast.Prefix, mk_local i e.epos); etype = basic.tint; epos = e.epos },
							old_len
						);
						etype = basic.tbool;
						epos = e.epos
					},
					{ eexpr = TBlock [
						{
							eexpr = TVar(obj_v, Some (mk_cast t_dynamic { eexpr = TArray(e, mk_local i e.epos); etype = old_param; epos = e.epos }));
							etype = basic.tvoid;
							epos = e.epos
						};
						{
							eexpr = TIf({
								eexpr = TBinop(Ast.OpNotEq, mk_local obj_v e.epos, null e.etype e.epos);
								etype = basic.tbool;
								epos = e.epos
							},
							{
								eexpr = TBinop(
									Ast.OpAssign,
									{ eexpr = TArray(mk_local new_v e.epos, mk_local i e.epos); etype = new_param; epos = e.epos },
									mk_cast new_param (mk_local obj_v e.epos)
								);
								etype = new_param;
								epos = e.epos
							},
							None);
							etype = basic.tvoid;
							epos = e.epos
						}
					]; etype = basic.tvoid; epos = e.epos },
					Ast.NormalWhile
				);
				etype = basic.tvoid;
				epos = e.epos;
			};
			mk_local new_v e.epos
		] in
		{
			eexpr = TIf(
				check_null,
				{
					eexpr = TBlock(block);
					etype = to_t;
					epos = e.epos;
				},
				Some(null new_v.v_type e.epos)
			);
			etype = to_t;
			epos = e.epos;
		}
	in

	Hashtbl.add gen.gtparam_cast (["cs"], "NativeArray") gtparam_cast_native_array
	(* end set gtparam_cast *)

let connecting_string = "?" (* ? see list here http://www.fileformat.info/info/unicode/category/index.htm and here for C# http://msdn.microsoft.com/en-us/library/aa664670.aspx *)
let default_package = "cs" (* I'm having this separated as I'm still not happy with having a cs package. Maybe dotnet would be better? *)
let strict_mode = ref false (* strict mode is so we can check for unexpected information *)

(* reserved c# words *)
let reserved = let res = Hashtbl.create 120 in
	List.iter (fun lst -> Hashtbl.add res lst ("@" ^ lst)) ["abstract"; "as"; "base"; "bool"; "break"; "byte"; "case"; "catch"; "char"; "checked"; "class";
		"const"; "continue"; "decimal"; "default"; "delegate"; "do"; "double"; "else"; "enum"; "event"; "explicit";
		"extern"; "false"; "finally"; "fixed"; "float"; "for"; "foreach"; "goto"; "if"; "implicit"; "in"; "int";
		"interface"; "internal"; "is"; "lock"; "long"; "namespace"; "new"; "null"; "object"; "operator"; "out"; "override";
		"params"; "private"; "protected"; "public"; "readonly"; "ref"; "return"; "sbyte"; "sealed"; "short"; "sizeof";
		"stackalloc"; "static"; "string"; "struct"; "switch"; "this"; "throw"; "true"; "try"; "typeof"; "uint"; "ulong";
		"unchecked"; "unsafe"; "ushort"; "using"; "virtual"; "volatile"; "void"; "while"; "add"; "ascending"; "by"; "descending";
		"dynamic"; "equals"; "from"; "get"; "global"; "group"; "into"; "join"; "let"; "on"; "orderby"; "partial";
		"remove"; "select"; "set"; "value"; "var"; "where"; "yield"];
	res

let dynamic_anon = TAnon( { a_fields = PMap.empty; a_status = ref Closed } )

let rec get_class_modifiers meta cl_type cl_access cl_modifiers =
	match meta with
		| [] -> cl_type,cl_access,cl_modifiers
		| (Meta.Struct,[],_) :: meta -> get_class_modifiers meta "struct" cl_access cl_modifiers
		| (Meta.Protected,[],_) :: meta -> get_class_modifiers meta cl_type "protected" cl_modifiers
		| (Meta.Internal,[],_) :: meta -> get_class_modifiers meta cl_type "internal" cl_modifiers
		(* no abstract for now | (":abstract",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("abstract" :: cl_modifiers)
		| (":static",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("static" :: cl_modifiers) TODO: support those types *)
		| (Meta.Final,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("sealed" :: cl_modifiers)
		| (Meta.Unsafe,[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("unsafe" :: cl_modifiers)
		| _ :: meta -> get_class_modifiers meta cl_type cl_access cl_modifiers

let rec get_fun_modifiers meta access modifiers =
	match meta with
		| [] -> access,modifiers
		| (Meta.Protected,[],_) :: meta -> get_fun_modifiers meta "protected" modifiers
		| (Meta.Internal,[],_) :: meta -> get_fun_modifiers meta "internal" modifiers
		| (Meta.ReadOnly,[],_) :: meta -> get_fun_modifiers meta access ("readonly" :: modifiers)
		| (Meta.Unsafe,[],_) :: meta -> get_fun_modifiers meta access ("unsafe" :: modifiers)
		| (Meta.Volatile,[],_) :: meta -> get_fun_modifiers meta access ("volatile" :: modifiers)
		| (Meta.Custom ("?prop_impl" | ":cs_event_impl"),[],_) :: meta -> get_fun_modifiers meta "private" modifiers
		| _ :: meta -> get_fun_modifiers meta access modifiers

let generate con =
	(try
		let gen = new_ctx con in
		let basic = con.basic in

		if Common.defined_value con Define.Dce = "no" then begin
			let m = { null_module with m_id = alloc_mid(); m_path = ["haxe";"lang"],"DceNo" } in
			let cl = mk_class m (["haxe";"lang"],"DceNo") null_pos in
			gen.gtypes_list <- (TClassDecl cl) :: gen.gtypes_list;
			Hashtbl.add gen.gtypes cl.cl_path (TClassDecl cl)
		end;

		(* make the basic functions in C# *)
		let type_cl = get_cl ( get_type gen (["System"], "Type")) in
		let basic_fns =
		[
			mk_class_field "Equals" (TFun(["obj",false,t_dynamic], basic.tbool)) true null_pos (Method MethNormal) [];
			mk_class_field "ToString" (TFun([], basic.tstring)) true null_pos (Method MethNormal) [];
			mk_class_field "GetHashCode" (TFun([], basic.tint)) true null_pos (Method MethNormal) [];
			mk_class_field "GetType" (TFun([], TInst(type_cl, []))) true null_pos (Method MethNormal) [];
		] in
		List.iter (fun cf -> gen.gbase_class_fields <- PMap.add cf.cf_name cf gen.gbase_class_fields) basic_fns;

		let native_arr_cl = get_cl ( get_type gen (["cs"], "NativeArray") ) in
		gen.gclasses.nativearray <- (fun t -> TInst(native_arr_cl,[t]));
		gen.gclasses.nativearray_type <- (function TInst(_,[t]) -> t | _ -> assert false);
		gen.gclasses.nativearray_len <- (fun e p -> mk_field_access gen e "Length" p);

		let erase_generics = Common.defined gen.gcon Define.EraseGenerics in
		let fn_cl = get_cl (get_type gen (["haxe";"lang"],"Function")) in
		let null_t = if erase_generics then null_class else (get_cl (get_type gen (["haxe";"lang"],"Null")) ) in
		let runtime_cl = get_cl (get_type gen (["haxe";"lang"],"Runtime")) in
		let no_root = Common.defined gen.gcon Define.NoRoot in
		let change_id name = try
				Hashtbl.find reserved name
			with | Not_found ->
				let ret = String.concat "." (String.nsplit name "#") in
				List.hd (String.nsplit ret "`")
		in

		let change_clname n = change_id n in

		let change_ns_params_root md ns params =
			let ns,params = List.fold_left (fun (ns,params) nspart -> try
				let part, nparams = String.split nspart "`" in
				let nparams = int_of_string nparams in
				let rec loop i needed params =
					if i = nparams then
						(List.rev needed,params)
					else
						loop (i+1) (List.hd params :: needed) (List.tl params)
				in
				let needed,params = loop 0 [] params in
				let part = change_id part in
				(part ^ "<" ^ (String.concat ", " needed) ^ ">")::ns, params
			with _ -> (* Invalid_string / int_of_string *)
				(change_id nspart)::ns, params
			) ([],params) ns
			in
			List.rev ns,params
		in

		let change_ns_params md params ns = if no_root then (
			let needs_root md = is_hxgen md || match md with
				| TClassDecl cl when (Meta.has Meta.Enum cl.cl_meta) && (Meta.has Meta.CompilerGenerated cl.cl_meta) ->
					(* this will match our compiler-generated enum constructor classes *)
					true
				| _ ->
					false
			in
			match ns with
				| [] when needs_root md -> ["haxe";"root"], params
				| [s] when (t_infos md).mt_private && needs_root md -> ["haxe";"root";s], params
				| [] -> (match md with
					| TClassDecl { cl_path = ([],"Std" | [],"Math") } -> ["haxe";"root"], params
					| TClassDecl { cl_meta = m } when Meta.has Meta.Enum m -> ["haxe";"root"], params
					| _ -> [], params)
				| ns when params = [] -> List.map change_id ns, params
				| ns ->
					change_ns_params_root md ns params)
			else if params = [] then
				List.map change_id ns, params
			else
				change_ns_params_root md ns params
		in

		let change_ns md ns =
			let ns, _ = change_ns_params md [] ns in
			ns
		in

		let change_field = change_id in
		let write_id w name = write w (change_id name) in
		let write_field w name = write w (change_field name) in

		let ptr =
			if Common.defined gen.gcon Define.Unsafe then
				get_abstract (get_type gen (["cs"],"Pointer"))
			else
				null_abstract
		in

		let is_hxgeneric md =
			RealTypeParams.is_hxgeneric md
		in

		let rec field_is_hxgeneric e = match e.eexpr with
			| TParenthesis e | TMeta(_,e) -> field_is_hxgeneric e
			| TField(_, (FStatic(cl,_) | FInstance(cl,_,_)) ) ->
				(* print_endline ("is_hxgeneric " ^ s_type_path cl.cl_path ^ " : " ^ string_of_bool (is_hxgeneric (TClassDecl cl))); *)
				is_hxgeneric (TClassDecl cl)
			| _ -> true
		in

		gen.gfollow#add "follow_basic" PZero (fun t -> match t with
				| TAbstract ({ a_path = ([], "Bool") },[])
				| TAbstract ({ a_path = ([], "Void") },[])
				| TAbstract ({ a_path = ([],"Float") },[])
				| TAbstract ({ a_path = ([],"Int") },[])
				| TAbstract ({ a_path = [],"UInt" },[])
				| TType ({ t_path = ["cs"], "Int64" },[])
				| TAbstract ({ a_path = ["cs"], "Int64" },[])
				| TType ({ t_path = ["cs"],"UInt64" },[])
				| TAbstract ({ a_path = ["cs"],"UInt64" },[])
				| TType ({ t_path = ["cs"],"UInt8" },[])
				| TAbstract ({ a_path = ["cs"],"UInt8" },[])
				| TType ({ t_path = ["cs"],"Int8" },[])
				| TAbstract ({ a_path = ["cs"],"Int8" },[])
				| TType ({ t_path = ["cs"],"Int16" },[])
				| TAbstract ({ a_path = ["cs"],"Int16" },[])
				| TType ({ t_path = ["cs"],"UInt16" },[])
				| TAbstract ({ a_path = ["cs"],"UInt16" },[])
				| TType ({ t_path = ["cs"],"Char16" },[])
				| TAbstract ({ a_path = ["cs"],"Char16" },[])
				| TType ({ t_path = ["cs"],"Ref" },_)
				| TAbstract ({ a_path = ["cs"],"Ref" },_)
				| TType ({ t_path = ["cs"],"Out" },_)
				| TAbstract ({ a_path = ["cs"],"Out" },_)
				| TType ({ t_path = [],"Single" },[])
				| TAbstract ({ a_path = [],"Single" },[]) -> Some t
				| TAbstract (({ a_path = [],"Null" } as tab),[t2]) ->
						Some (TAbstract(tab,[follow (gen.gfollow#run_f t2)]))
				| TAbstract({ a_path = ["cs"],"PointerAccess" },[t]) ->
						Some (TAbstract(ptr,[t]))
				| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
						Some (gen.gfollow#run_f ( Abstract.get_underlying_type a pl) )
				| TAbstract( { a_path = ([], "EnumValue") }, _	)
				| TInst( { cl_path = ([], "EnumValue") }, _  ) -> Some t_dynamic
				| _ -> None);

		let module_s_params md params =
			let md = change_md md in
			let path = (t_infos md).mt_path in
			match path with
				| ([], "String") -> "string", params
				| ([], "Null") -> s_type_path (change_ns md ["haxe"; "lang"], change_clname "Null"), params
				| (ns,clname) ->
					let ns, params = change_ns_params md params ns in
					s_type_path (ns, change_clname clname), params
		in

		let module_s md =
			fst (module_s_params md [])
		in

		let ifaces = Hashtbl.create 1 in

		let ti64 = match ( get_type gen (["cs"], "Int64") ) with | TTypeDecl t -> TType(t,[]) | TAbstractDecl a -> TAbstract(a,[]) | _ -> assert false in

		let ttype = get_cl ( get_type gen (["System"], "Type") ) in

		let has_tdyn tl =
			List.exists (fun t -> match follow t with
			| TDynamic _ | TMono _ -> true
			| _ -> false
		) tl
		in

		let rec real_type t =
			let t = gen.gfollow#run_f t in
			let ret = match t with
				| TAbstract({ a_path = ([], "Null") }, [t]) ->
					(*
						Null<> handling is a little tricky.
						It will only change to haxe.lang.Null<> when the actual type is non-nullable or a type parameter
						It works on cases such as Hash<T> returning Null<T> since cast_detect will invoke real_type at the original type,
						Null<T>, which will then return the type haxe.lang.Null<>
					*)
					if erase_generics then
						if is_cs_basic_type t then
							t_dynamic
						else
							real_type t
					else
						(match real_type t with
							| TInst( { cl_kind = KTypeParameter _ }, _ ) -> TInst(null_t, [t])
							| t when is_cs_basic_type t -> TInst(null_t, [t])
							| _ -> real_type t)
				| TAbstract (a, pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					real_type (Abstract.get_underlying_type a pl)
				| TAbstract ({ a_path = (["cs";"_Flags"], "EnumUnderlying") }, [t]) ->
					real_type t
				| TInst( { cl_path = (["cs";"system"], "String") }, [] ) ->
					gen.gcon.basic.tstring;
				| TInst( { cl_path = (["haxe"], "Int32") }, [] ) -> gen.gcon.basic.tint
				| TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> ti64
				| TAbstract( { a_path = [],"Class" }, _ )
				| TAbstract( { a_path = [],"Enum" }, _ )
				| TAbstract( { a_path = ["haxe";"extern"],"Rest" }, _ )
				| TInst( { cl_path = ([], "Class") }, _ )
				| TInst( { cl_path = ([], "Enum") }, _ ) -> TInst(ttype,[])
				| TInst( ({ cl_kind = KTypeParameter _ } as cl), _ ) when erase_generics && not (Meta.has Meta.NativeGeneric cl.cl_meta) ->
					t_dynamic
				| TInst({ cl_kind = KExpr _ }, _) -> t_dynamic
				| TEnum(_, [])
				| TInst(_, []) -> t
				| TInst(cl, params) when
					has_tdyn params &&
					Hashtbl.mem ifaces cl.cl_path ->
						TInst(Hashtbl.find ifaces cl.cl_path, [])
				| TEnum(e, params) ->
					TEnum(e, List.map (fun _ -> t_dynamic) params)
				| TInst(cl, params) when Meta.has Meta.Enum cl.cl_meta ->
					TInst(cl, List.map (fun _ -> t_dynamic) params)
				| TInst(cl, params) -> TInst(cl, change_param_type (TClassDecl cl) params)
				| TAbstract _
				| TType _ -> t
				| TAnon (anon) when (match !(anon.a_status) with | Statics _ | EnumStatics _ | AbstractStatics _ -> true | _ -> false) -> t
				| TFun _ -> TInst(fn_cl,[])
				| _ -> t_dynamic
			in
			ret
		and

		(*
			On hxcs, the only type parameters allowed to be declared are the basic c# types.
			That's made like this to avoid casting problems when type parameters in this case
			add nothing to performance, since the memory layout is always the same.

			To avoid confusion between Generic<Dynamic> (which has a different meaning in hxcs AST),
			all those references are using dynamic_anon, which means Generic<{}>
		*)
		change_param_type md tl =
			let types = match md with
				| TClassDecl c -> c.cl_params
				| TEnumDecl e -> []
				| TAbstractDecl a -> a.a_params
				| TTypeDecl t -> t.t_params
			in
			let is_hxgeneric = if types = [] then is_hxgen md else (RealTypeParams.is_hxgeneric md) in
			let ret t =
				let t_changed = real_type t in
				match is_hxgeneric, t_changed with
				| false, _ -> t
				(*
					Because Null<> types need a special compiler treatment for many operations (e.g. boxing/unboxing),
					Null<> type parameters will be transformed into Dynamic.
				*)
				| true, TInst ( { cl_path = (["haxe";"lang"], "Null") }, _ ) -> dynamic_anon
				| true, TInst ( { cl_kind = KTypeParameter _ }, _ ) -> t
				| true, TInst _
				| true, TEnum _
				| true, TAbstract _ when is_cs_basic_type t_changed -> t
				| true, TDynamic _ -> t
				| true, x ->
					dynamic_anon
			in
			if is_hxgeneric && (erase_generics || List.exists (fun t -> match follow t with | TDynamic _ -> true | _ -> false) tl) then
				List.map (fun _ -> t_dynamic) tl
			else
				List.map ret tl
		in

		let is_dynamic t = match real_type t with
			| TMono _ | TDynamic _
			| TInst({ cl_kind = KTypeParameter _ }, _) -> true
			| TAnon anon ->
				(match !(anon.a_status) with
					| EnumStatics _ | Statics _ -> false
					| _ -> true
				)
			| _ -> false
		in

		let rec t_s t =
			match real_type t with
				(* basic types *)
				| TAbstract ({ a_path = ([], "Bool") },[]) -> "bool"
				| TAbstract ({ a_path = ([], "Void") },[]) -> "object"
				| TAbstract ({ a_path = ([],"Float") },[]) -> "double"
				| TAbstract ({ a_path = ([],"Int") },[]) -> "int"
				| TAbstract ({ a_path = [],"UInt" },[]) -> "uint"
				| TType ({ t_path = ["cs"], "Int64" },[])
				| TAbstract ({ a_path = ["cs"], "Int64" },[]) -> "long"
				| TType ({ t_path = ["cs"],"UInt64" },[])
				| TAbstract ({ a_path = ["cs"],"UInt64" },[]) -> "ulong"
				| TType ({ t_path = ["cs"],"UInt8" },[])
				| TAbstract ({ a_path = ["cs"],"UInt8" },[]) -> "byte"
				| TType ({ t_path = ["cs"],"Int8" },[])
				| TAbstract ({ a_path = ["cs"],"Int8" },[]) -> "sbyte"
				| TType ({ t_path = ["cs"],"Int16" },[])
				| TAbstract ({ a_path = ["cs"],"Int16" },[]) -> "short"
				| TType ({ t_path = ["cs"],"UInt16" },[])
				| TAbstract ({ a_path = ["cs"],"UInt16" },[]) -> "ushort"
				| TType ({ t_path = ["cs"],"Char16" },[])
				| TAbstract ({ a_path = ["cs"],"Char16" },[]) -> "char"
				| TType ({ t_path = [],"Single" },[])
				| TAbstract ({ a_path = [],"Single" },[]) -> "float"
				| TInst ({ cl_path = ["haxe"],"Int32" },[])
				| TAbstract ({ a_path = ["haxe"],"Int32" },[]) -> "int"
				| TInst ({ cl_path = ["haxe"],"Int64" },[])
				| TAbstract ({ a_path = ["haxe"],"Int64" },[]) -> "long"
				| TInst ({ cl_path = ([], "Dynamic") },_)
				| TAbstract ({ a_path = ([], "Dynamic") },_) -> "object"
				| TType ({ t_path = ["cs"],"Out" },[t])
				| TAbstract ({ a_path = ["cs"],"Out" },[t])
				| TType ({ t_path = ["cs"],"Ref" },[t])
				| TAbstract ({ a_path = ["cs"],"Ref" },[t]) -> t_s t
				| TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
					let rec check_t_s t =
						match real_type t with
							| TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
								(check_t_s param) ^ "[]"
							| _ -> t_s (run_follow gen t)
					in
					(check_t_s param) ^ "[]"
				| TInst({ cl_path = (["cs"], "Pointer") },[t])
				| TAbstract({ a_path = (["cs"], "Pointer") },[t])->
					let ret = t_s t in
					(if ret = "object" then "void" else ret) ^ "*"
				(* end of basic types *)
				| TInst ({ cl_kind = KTypeParameter _; cl_path=p }, []) -> snd p
				| TMono r -> (match !r with | None -> "object" | Some t -> t_s (run_follow gen t))
				| TInst ({ cl_path = [], "String" }, []) -> "string"
				| TEnum (e, params) -> ("global::" ^ (module_s (TEnumDecl e)))
				| TInst (cl, _ :: _) when Meta.has Meta.Enum cl.cl_meta ->
					"global::" ^ module_s (TClassDecl cl)
				| TInst (({ cl_path = p } as cl), params) -> (path_param_s (TClassDecl cl) p params)
				| TType (({ t_path = p } as t), params) -> (path_param_s (TTypeDecl t) p params)
				| TAnon (anon) ->
					(match !(anon.a_status) with
						| Statics _ | EnumStatics _ -> "System.Type"
						| _ -> "object")
				| TDynamic _ -> "object"
				| TAbstract(a,pl) when not (Meta.has Meta.CoreType a.a_meta) ->
					t_s (Abstract.get_underlying_type a pl)
				(* No Lazy type nor Function type made. That's because function types will be at this point be converted into other types *)
				| _ -> if !strict_mode then begin trace ("[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"); assert false end else "[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"

		and path_param_s md path params =
				match params with
					| [] -> "global::" ^ module_s md
					| _ when erase_generics && is_hxgeneric md ->
						"global::" ^ module_s md
					| _ ->
						let params = (List.map (fun t -> t_s t) (change_param_type md params)) in
						let str,params = module_s_params md params in
						if params = [] then
							"global::" ^ str
						else
							sprintf "global::%s<%s>" str (String.concat ", " params)
		in

		let rett_s t =
			match t with
				| TAbstract ({ a_path = ([], "Void") },[]) -> "void"
				| _ -> t_s t
		in

		let escape ichar b =
			match ichar with
				| 92 (* \ *) -> Buffer.add_string b "\\\\"
				| 39 (* ' *) -> Buffer.add_string b "\\\'"
				| 34 -> Buffer.add_string b "\\\""
				| 13 (* \r *) -> Buffer.add_string b "\\r"
				| 10 (* \n *) -> Buffer.add_string b "\\n"
				| 9 (* \t *) -> Buffer.add_string b "\\t"
				| c when c < 32 || (c >= 127 && c <= 0xFFFF) -> Buffer.add_string b (Printf.sprintf "\\u%.4x" c)
				| c when c > 0xFFFF -> Buffer.add_string b (Printf.sprintf "\\U%.8x" c)
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
				| TBlock _ | TFor _ | TSwitch _ | TTry _ | TIf _ -> false
				| TWhile (_,_,flag) when flag = Ast.NormalWhile -> false
				| _ -> true
		in

		let in_value = ref false in

		let rec md_s md =
			let md = follow_module (gen.gfollow#run_f) md in
			match md with
				| TClassDecl ({ cl_params = [] } as cl) ->
					t_s (TInst(cl,[]))
				| TClassDecl (cl) when not (is_hxgen md) ->
					t_s (TInst(cl,List.map (fun t -> t_dynamic) cl.cl_params))
				| TEnumDecl ({ e_params = [] } as e) ->
					t_s (TEnum(e,[]))
				| TEnumDecl (e) when not (is_hxgen md) ->
					t_s (TEnum(e,List.map (fun t -> t_dynamic) e.e_params))
				| TClassDecl cl ->
					t_s (TInst(cl,[]))
				| TEnumDecl e ->
					t_s (TEnum(e,[]))
				| TTypeDecl t ->
					t_s (TType(t, List.map (fun t -> t_dynamic) t.t_params))
				| TAbstractDecl a ->
					t_s (TAbstract(a, List.map(fun t -> t_dynamic) a.a_params))
		in

		let rec ensure_local e explain =
			match e.eexpr with
				| TLocal _ -> e
				| TCast(e,_)
				| TParenthesis e | TMeta(_,e) -> ensure_local e explain
				| _ -> gen.gcon.error ("This function argument " ^ explain ^ " must be a local variable.") e.epos; e
		in

		let rec ensure_refout e explain =
			match e.eexpr with
				| TField _ | TLocal _ -> e
				| TCast(e,_)
				| TParenthesis e | TMeta(_,e) -> ensure_refout e explain
				| _ -> gen.gcon.error ("This function argument " ^ explain ^ " must be a local variable.") e.epos; e
		in

		let last_line = ref (-1) in
		let begin_block w = write w "{"; push_indent w; newline w; last_line := -1 in
		let end_block w = pop_indent w; (if w.sw_has_content then newline w); write w "}"; newline w; last_line := -1 in
		let skip_line_directives = (not gen.gcon.debug && not (Common.defined gen.gcon Define.NoCompilation)) || Common.defined gen.gcon Define.RealPosition in
		let line_directive =
			if skip_line_directives then
				fun w p -> ()
			else fun w p ->
				if p.pfile <> null_pos.pfile then (* Compiler Error CS1560 https://msdn.microsoft.com/en-us/library/z3t5e5sw(v=vs.90).aspx *)
				let cur_line = Lexer.get_error_line p in
				let file = Path.get_full_path p.pfile in
				if cur_line <> ((!last_line)+1) then
					let line = Ast.s_escape file in
					if String.length line <= 256 then
						begin print w "#line %d \"%s\"" cur_line line; newline w end
					else (* Compiler Error CS1560 https://msdn.microsoft.com/en-us/library/z3t5e5sw(v=vs.90).aspx *)
						begin print w "//line %d \"%s\"" cur_line line; newline w end;
				last_line := cur_line
		in
		let line_reset_directive =
			if skip_line_directives then
				fun w -> ()
			else fun w ->
				print w "#line default"
		in

		let rec extract_tparams params el =
			match el with
				| ({ eexpr = TIdent "$type_param" } as tp) :: tl ->
					extract_tparams (tp.etype :: params) tl
				| _ -> (params, el)
		in

		let is_extern_prop t name = match follow (run_follow gen t), field_access gen t name with
			| TInst({ cl_interface = true; cl_extern = true } as cl, _), FNotFound ->
				not (is_hxgen (TClassDecl cl))
			| _, FClassField(_,_,decl,v,_,t,_) ->
				not (Type.is_physical_field v) && (Meta.has Meta.Property v.cf_meta || (decl.cl_extern && not (is_hxgen (TClassDecl decl))))
			| _ -> false
		in

		let is_event t name = match follow (run_follow gen t), field_access gen t name with
			| TInst({ cl_interface = true; cl_extern = true } as cl, _), FNotFound ->
				not (is_hxgen (TClassDecl cl))
			| _, FClassField(_,_,decl,v,_,_,_) ->
				Meta.has Meta.Event v.cf_meta
			| _ -> false
		in

		let extract_statements expr =
			let ret = ref [] in
			let rec loop expr = match expr.eexpr with
				| TCall ({ eexpr = TIdent ("__is__" | "__typeof__" | "__array__" | "__sizeof__" | "__delegate__")}, el) ->
					List.iter loop el
				| TNew ({ cl_path = (["cs"], "NativeArray") }, params, [ size ]) ->
					()
				| TUnop (Ast.Increment, _, _)
				| TUnop (Ast.Decrement, _, _)
				| TBinop (Ast.OpAssign, _, _)
				| TBinop (Ast.OpAssignOp _, _, _)
				| TIdent "__fallback__"
				| TIdent "__sbreak__" ->
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
			last_line := -1;
			in_value := false;
			let rec expr_s w e =
				let was_in_value = !in_value in
				in_value := true;
				(match e.eexpr with
					| TCall({ eexpr = TField(ef,f) }, (_ :: _ as args) ) when (field_name f) = "get_Item" ->
						expr_s w ef;
						write w "[";
						let first = ref true in
						List.iter (fun f ->
							if !first then first := false else write w ", ";
							expr_s w f
						) args;
						write w "]"
					| TCall({ eexpr = TField(ef,f) }, (_ :: _ :: _ as args) ) when (field_name f) = "set_Item" ->
						expr_s w ef;
						write w "[";
						let args, value = match List.rev args with
							| v :: args -> List.rev args, v
							| _ -> assert false
						in
						let first = ref true in
						List.iter (fun f ->
							if !first then first := false else write w ", ";
							expr_s w f
						) args;
						write w "] = ";
						expr_s w value
					| TCall( ({ eexpr = TField(ef,f) } as e), [ev] ) when String.starts_with (field_name f) "add_" ->
						let name = field_name f in
						let propname = String.sub name 4 (String.length name - 4) in
						if is_event (gen.greal_type ef.etype) propname then begin
							expr_s w ef;
							write w ".";
							write_field w propname;
							write w " += ";
							expr_s w ev
						end else
							do_call w e [ev]
					| TCall( ({ eexpr = TField(ef,f) } as e), [ev] ) when String.starts_with (field_name f) "remove_" ->
						let name = field_name f in
						let propname = String.sub name 7 (String.length name - 7) in
						if is_event (gen.greal_type ef.etype) propname then begin
							expr_s w ef;
							write w ".";
							write_field w propname;
							write w " -= ";
							expr_s w ev
						end else
							do_call w e [ev]
					| TCall( ({ eexpr = TField(ef,f) } as e), [] ) when String.starts_with (field_name f) "get_" ->
						let name = field_name f in
						let propname = String.sub name 4 (String.length name - 4) in
						if is_extern_prop (gen.greal_type ef.etype) propname then begin
							expr_s w ef;
							write w ".";
							write_field w propname
						end else
							do_call w e []
					| TCall( ({ eexpr = TField(ef,f) } as e), [v] ) when String.starts_with (field_name f) "set_" ->
						let name = field_name f in
						let propname = String.sub name 4 (String.length name - 4) in
						if is_extern_prop (gen.greal_type ef.etype) propname then begin
							expr_s w ef;
							write w ".";
							write_field w propname;
							write w " = ";
							expr_s w v
						end else
							do_call w e [v]
					| TField (e, (FStatic(_, cf) | FInstance(_, _, cf))) when Meta.has Meta.Native cf.cf_meta ->
						let rec loop meta = match meta with
							| (Meta.Native, [EConst (String (s,_)), _],_) :: _ ->
								expr_s w e; write w "."; write_field w s
							| _ :: tl -> loop tl
							| [] -> expr_s w e; write w "."; write_field w (cf.cf_name)
						in
						loop cf.cf_meta
					| TConst c ->
						(match c with
							| TInt i32 ->
								write w (Int32.to_string i32);
								(* these suffixes won't work because of the cast detector which will set each constant to its expected type *)
								(*match real_type e.etype with
									| TType( { t_path = (["haxe";"_Int64"], "NativeInt64") }, [] ) -> write w "L";
									| _ -> ()
								*)
							| TFloat s ->
								write w s;
								(if String.get s (String.length s - 1) = '.' then write w "0");
								(*match real_type e.etype with
									| TType( { t_path = ([], "Single") }, [] ) -> write w "f"
									| _ -> ()
								*)
							| TString s ->
								write w "\"";
								write w (escape s);
								write w "\""
							| TBool b -> write w (if b then "true" else "false")
							| TNull when is_cs_basic_type e.etype || is_tparam e.etype ->
								write w "default(";
								write w (t_s e.etype);
								write w ")"
							| TNull -> write w "null"
							| TThis -> write w "this"
							| TSuper -> write w "base")
					| TCast({ eexpr = TConst(TNull) }, _) ->
								write w "default(";
								write w (t_s e.etype);
								write w ")"
					| TIdent "__sbreak__" -> write w "break"
					| TIdent "__undefined__" ->
						write w (t_s (TInst(runtime_cl, List.map (fun _ -> t_dynamic) runtime_cl.cl_params)));
						write w ".undefined";
					| TIdent "__typeof__" -> write w "typeof"
					| TIdent "__sizeof__" -> write w "sizeof"
					| TLocal var ->
						write_id w var.v_name
					| TField (_, FEnum(e, ef)) ->
						let s = ef.ef_name in
						print w "%s." ("global::" ^ module_s (TEnumDecl e)); write_field w s
					| TArray (e1, e2) ->
						expr_s w e1; write w "["; expr_s w e2; write w "]"
					| TBinop ((Ast.OpAssign as op), e1, e2)
					| TBinop ((Ast.OpAssignOp _ as op), e1, e2) ->
						expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2
					| TBinop (op, e1, e2) ->
						write w "( ";
						expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2;
						write w " )"
					| TField ({ eexpr = TTypeExpr mt }, s) ->
						(match mt with
							| TClassDecl { cl_path = (["haxe"], "Int64") } -> write w ("global::" ^ module_s mt)
							| TClassDecl { cl_path = (["haxe"], "Int32") } -> write w ("global::" ^ module_s mt)
							| TClassDecl { cl_interface = true } ->
									write w ("global::" ^ module_s mt);
									write w "__Statics_";
							| TClassDecl cl -> write w (t_s (TInst(cl, List.map (fun _ -> t_empty) cl.cl_params)))
							| TEnumDecl en -> write w (t_s (TEnum(en, List.map (fun _ -> t_empty) en.e_params)))
							| TTypeDecl td -> write w (t_s (gen.gfollow#run_f (TType(td, List.map (fun _ -> t_empty) td.t_params))))
							| TAbstractDecl a -> write w (t_s (TAbstract(a, List.map (fun _ -> t_empty) a.a_params)))
						);
						write w ".";
						write_field w (field_name s)
					| TField (e, s) when is_pointer gen e.etype ->
						(* take off the extra cast if possible *)
						let e = match e.eexpr with
							| TCast(e1,_) when CastDetect.type_iseq gen e.etype e1.etype ->
								e1
							| _ -> e
						in
						expr_s w e; write w "->"; write_field w (field_name s)
					| TField (e, s) ->
						expr_s w e; write w "."; write_field w (field_name s)
					| TTypeExpr mt ->
						(match change_md mt with
							| TClassDecl { cl_path = (["haxe"], "Int64") } -> write w ("global::" ^ module_s mt)
							| TClassDecl { cl_path = (["haxe"], "Int32") } -> write w ("global::" ^ module_s mt)
							| TClassDecl cl -> write w (t_s (TInst(cl, List.map (fun _ -> t_empty) cl.cl_params)));
							| TEnumDecl en -> write w (t_s (TEnum(en, List.map (fun _ -> t_empty) en.e_params)))
							| TTypeDecl td -> write w (t_s (gen.gfollow#run_f (TType(td, List.map (fun _ -> t_empty) td.t_params))))
							| TAbstractDecl a -> write w (t_s (TAbstract(a, List.map (fun _ -> t_empty) a.a_params)))
						)
					| TParenthesis e ->
						write w "("; expr_s w e; write w ")"
					| TMeta ((Meta.LoopLabel,[(EConst(Int n),_)],_), e) ->
						(match e.eexpr with
						| TFor _ | TWhile _ ->
							expr_s w e;
							print w "label%s: {}" n
						| TBreak -> print w "goto label%s" n
						| _ -> assert false)
					| TMeta (_,e) ->
								expr_s w e
					| TArrayDecl el
					| TCall ({ eexpr = TIdent "__array__" }, el)
					| TCall ({ eexpr = TField(_, FStatic({ cl_path = (["cs"],"NativeArray") }, { cf_name = "make" })) }, el) ->
						let _, el = extract_tparams [] el in
						print w "new %s" (t_s e.etype);
						write w "{";
						ignore (List.fold_left (fun acc e ->
							(if acc <> 0 then write w ", ");
							expr_s w e;
							acc + 1
						) 0 el);
						write w "}"
					| TCall ({ eexpr = TIdent "__delegate__" }, [del]) ->
						expr_s w del
					| TCall ({ eexpr = TIdent "__is__" }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
						write w "( ";
						expr_s w expr;
						write w " is ";
						write w (md_s md);
						write w " )"
					| TCall ({ eexpr = TIdent "__as__" }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
						write w "( ";
						expr_s w expr;
						write w " as ";
						write w (md_s md);
						write w " )"
					| TCall ({ eexpr = TIdent "__as__" }, expr :: _ ) ->
						write w "( ";
						expr_s w expr;
						write w " as ";
						write w (t_s e.etype);
						write w " )";
					| TCall ({ eexpr = TIdent "__cs__" }, [ { eexpr = TConst(TString(s)) } ] ) ->
						write w s
					| TCall ({ eexpr = TIdent "__cs__" }, { eexpr = TConst(TString(s)) } :: tl ) ->
						Codegen.interpolate_code gen.gcon s tl (write w) (expr_s w) e.epos
					| TCall ({ eexpr = TIdent "__stackalloc__" }, [ e ] ) ->
						write w "stackalloc byte[";
						expr_s w e;
						write w "]"
					| TCall ({ eexpr = TIdent "__unsafe__" }, [ e ] ) ->
						write w "unsafe";
						expr_s w (mk_block e)
					| TCall ({ eexpr = TIdent "__checked__" }, [ e ] ) ->
						write w "checked";
						expr_s w (mk_block e)
					| TCall ({ eexpr = TIdent "__lock__" }, [ eobj; eblock ] ) ->
						write w "lock(";
						expr_s w eobj;
						write w ")";
						expr_s w (mk_block eblock)
					| TCall ({ eexpr = TIdent "__fixed__" }, [ e ] ) ->
						let fixeds = ref [] in
						let rec loop = function
							| ({ eexpr = TVar(v, Some(e) ) } as expr) :: tl when is_pointer gen v.v_type ->
								let e = match get_ptr e with
									| None -> e
									| Some e -> e
								in
								fixeds := (v,e,expr) :: !fixeds;
								loop tl;
							| el when !fixeds <> [] ->
								let rec loop fx acc = match fx with
									| (v,e,expr) :: tl ->
										write w "fixed(";
										let vf = mk_temp "fixed" v.v_type in
										expr_s w { expr with eexpr = TVar(vf, Some e) };
										write w ") ";
										begin_block w;
										expr_s w { expr with eexpr = TVar(v, Some (mk_local vf expr.epos)) };
										write w ";";
										newline w;
										loop tl (acc + 1)
									| [] -> acc
								in
								let nblocks = loop (List.rev !fixeds) 0 in
								in_value := false;
								expr_s w { e with eexpr = TBlock el };
								for i = 1 to nblocks do
									end_block w
								done
							| _ ->
								trace (debug_expr e);
								gen.gcon.error "Invalid 'fixed' keyword format" e.epos
						in
						(match e.eexpr with
							| TBlock bl -> loop bl
							| _ ->
								trace "not block";
								trace (debug_expr e);
								gen.gcon.error "Invalid 'fixed' keyword format" e.epos
						)
					| TCall ({ eexpr = TIdent "__addressOf__" }, [ e ] ) ->
						let e = ensure_local e "for addressOf" in
						write w "&";
						expr_s w e
					| TCall ({ eexpr = TIdent "__valueOf__" }, [ e ] ) ->
						write w "*(";
						expr_s w e;
						write w ")"
					(* operator overloading handling *)
					| TCall({ eexpr = TField(ef, FInstance(cl,_,{ cf_name = "__get" })) }, [idx]) when not (is_hxgen (TClassDecl cl)) ->
						expr_s w { e with eexpr = TArray(ef, idx) }
					| TCall({ eexpr = TField(ef, FInstance(cl,_,{ cf_name = "__set" })) }, [idx; v]) when not (is_hxgen (TClassDecl cl)) ->
						expr_s w { e with eexpr = TBinop(Ast.OpAssign, { e with eexpr = TArray(ef, idx) }, v) }
					| TCall({ eexpr = TField(ef, FStatic(_,cf)) }, el) when PMap.mem cf.cf_name binops_names ->
						let _, elr = extract_tparams [] el in
						(match elr with
						| [e1;e2] ->
							expr_s w { e with eexpr = TBinop(PMap.find cf.cf_name binops_names, e1, e2) }
						| _ -> do_call w e el)
					| TCall({ eexpr = TField(ef, FStatic(_,cf)) }, el) when PMap.mem cf.cf_name unops_names ->
						(match extract_tparams [] el with
						| _, [e1] ->
							expr_s w { e with eexpr = TUnop(PMap.find cf.cf_name unops_names, Ast.Prefix,e1) }
						| _ -> do_call w e el)
					| TCall (e, el) ->
						do_call w e el
					| TNew (({ cl_path = (["cs"], "NativeArray") } as cl), params, [ size ]) ->
						let rec check_t_s t times =
							match real_type t with
								| TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
									(check_t_s param (times+1))
								| _ ->
									print w "new %s[" (t_s (run_follow gen t));
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
						write w (t_s (TInst(cl, [])));
						write w "(";
						ignore (List.fold_left (fun acc e ->
							(if acc <> 0 then write w ", ");
							expr_s w e;
							acc + 1
						) 0 el);
						write w ")"
					| TNew ({ cl_kind = KTypeParameter _ } as cl, params, el) ->
						print w "default(%s) /* This code should never be reached. It was produced by the use of @:generic on a new type parameter instance: %s */" (t_s (TInst(cl,params))) (path_param_s (TClassDecl cl) cl.cl_path params)
					| TNew (cl, params, el) ->
						write w "new ";
						write w (path_param_s (TClassDecl cl) cl.cl_path params);
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
						print w "%s " (t_s var.v_type);
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
								line_directive w e.epos;
								in_value := false;
								expr_s w e;
								(if has_semicolon e then write w ";");
								newline w
							) (extract_statements e)
						) el;
						end_block w
					| TIf (econd, e1, Some(eelse)) when was_in_value ->
						let base = t_s e.etype in
						write w "( ";
						expr_s w (mk_paren econd);
						write w " ? ";
						if t_s e1.etype <> base then
							expr_s w (mk_cast e.etype e1)
						else
							expr_s w (mk_paren e1);

						write w " : ";
						if t_s eelse.etype <> base then
							expr_s w (mk_cast e.etype eelse)
						else
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
								write w "else ";
								in_value := false;
								let e = match e.eexpr with
									| TIf _ -> e
									| TBlock [{eexpr = TIf _} as e] -> e
									| _ -> mk_block e
								in
								expr_s w e
						)
					| TWhile (econd, eblock, flag) ->
						(match flag with
							| Ast.NormalWhile ->
								write w "while ";
								expr_s w (mk_paren econd);
								write w " ";
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
						write w " ";
						begin_block w;
						List.iter (fun (el, e) ->
							List.iter (fun e ->
								write w "case ";
								in_value := true;
								expr_s w e;
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
						List.iter (fun (var, e) ->
							print w "catch (%s %s)" (t_s var.v_type) (var.v_name);
							in_value := false;
							expr_s w (mk_block e);
							newline w
						) ve_l
					| TReturn eopt ->
						write w "return";
						if is_some eopt then (write w " "; expr_s w (get eopt))
					| TBreak -> write w "break"
					| TContinue -> write w "continue"
					| TThrow { eexpr = TIdent "__rethrow__" } ->
						write w "throw"
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
								print w "((%s) (" (t_s e.etype);
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
					| TEnumIndex _ -> write w "[ enum index not supported ]"; if !strict_mode then assert false
					| TIdent s -> write w "[ ident not supported ]"; if !strict_mode then assert false
			)
			and do_call w e el =
				let params, el = extract_tparams [] el in
				let params = List.rev params in

				expr_s w e;

				(match params with
					| _ :: _ when not (erase_generics && field_is_hxgeneric e) ->
						let md = match e.eexpr with
							| TField(ef, _) ->
								t_to_md (run_follow gen ef.etype)
							| _ -> assert false
						in
						write w "<";
						ignore (List.fold_left (fun acc t ->
							(if acc <> 0 then write w ", ");
							write w (t_s t);
							acc + 1
						) 0 (change_param_type md params));
						write w ">"
					| _ -> ()
				);

				let rec loop acc elist tlist =
					match elist, tlist with
						| e :: etl, (_,_,t) :: ttl ->
							(if acc <> 0 then write w ", ");
							(match real_type t with
								| TType({ t_path = (["cs"], "Ref") }, _)
								| TAbstract ({ a_path = (["cs"], "Ref") },_) ->
									let e = ensure_refout e "of type cs.Ref" in
									write w "ref ";
									expr_s w e
								| TType({ t_path = (["cs"], "Out") }, _)
								| TAbstract ({ a_path = (["cs"], "Out") },_) ->
									let e = ensure_refout e "of type cs.Out" in
									write w "out ";
									expr_s w e
								| _ ->
									expr_s w e
							);
							loop (acc + 1) etl ttl
						| e :: etl, [] ->
							(if acc <> 0 then write w ", ");
							expr_s w e;
							loop (acc + 1) etl []
						| _ -> ()
				in
				write w "(";
				let ft = match follow e.etype with
					| TFun(args,_) -> args
					| _ -> []
				in

				loop 0 el ft;

				write w ")"
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
				| String (s,_) ->
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
				write w "new[] {";
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

		let gen_attributes w metadata =
			List.iter (function
				| Meta.Meta, [EConst(String (s,_)), _], _ ->
					write w "[";
					write w s;
					write w "]";
					newline w
				| Meta.Meta, [meta], _ ->
					write w "[";
					gen_spart w meta;
					write w "]";
					newline w
				| _ -> ()
			) metadata
		in

		let gen_nocompletion w metadata =
			if Meta.has Meta.NoCompletion metadata then begin
				write w "[global::System.ComponentModel.EditorBrowsable(global::System.ComponentModel.EditorBrowsableState.Never)]";
				newline w
			end;
		in

		let argt_s t =
			let w = new_source_writer () in
			let rec run t =
				match t with
					| TType (tdef,p) ->
						gen_attributes w tdef.t_meta;
						run (follow_once t)
					| TMono r ->
						(match !r with
						| Some t -> run t
						| _ -> () (* avoid infinite loop / should be the same in this context *))
					| TLazy f ->
						run (lazy_type f)
					| _ -> ()
			in
			run t;
			let ret = match run_follow gen t with
				| TType ({ t_path = (["cs"], "Ref") }, [t])
				| TAbstract ({ a_path = (["cs"], "Ref") },[t]) -> "ref " ^ t_s t
				| TType ({ t_path = (["cs"], "Out") }, [t])
				| TAbstract ({ a_path = (["cs"], "Out") },[t]) -> "out " ^ t_s t
				| t -> t_s t
			in
			let c = contents w in
			if c <> "" then
				c ^ " " ^ ret
			else
				ret
		in

		let get_string_params cl cl_params =
			let hxgen = is_hxgen (TClassDecl cl) in
			match cl_params with
				| (_ :: _) when not (erase_generics && is_hxgeneric (TClassDecl cl)) ->
					let get_param_name t = match follow t with TInst(cl, _) -> snd cl.cl_path | _ -> assert false in
					let params = sprintf "<%s>" (String.concat ", " (List.map (fun (_, tcl) -> get_param_name tcl) cl_params)) in
					let params_extends =
						if hxgen
						(* this is temprorary, see https://github.com/HaxeFoundation/haxe/issues/3526 *)
						|| not (Meta.has (Meta.Custom ":nativeTypeConstraints") cl.cl_meta)
						then
							[""]
						else
							List.fold_left (fun acc (name, t) ->
								match run_follow gen t with
									| TInst({cl_kind = KTypeParameter constraints}, _) when constraints <> [] ->
										(* base class should come before interface constraints *)
										let base_class_constraints = ref [] in
										let other_constraints = List.fold_left (fun acc t ->
											match follow t with
												(* string is implicitly sealed, maybe haxe should have it final as well *)
												| TInst ({ cl_path=[],"String" }, []) ->
													acc

												(* non-sealed class *)
												| TInst ({ cl_interface = false; cl_meta = meta},_) when not (Meta.has Meta.Final meta) ->
													base_class_constraints := (t_s t) :: !base_class_constraints;
													acc;

												(* interface *)
												| TInst ({ cl_interface = true}, _) ->
													(t_s t) :: acc

												(* skip anything other *)
												| _ ->
													acc
										) [] constraints in

										let s_constraints = (!base_class_constraints @ other_constraints) in
										if s_constraints <> [] then
											(sprintf " where %s : %s" (get_param_name t) (String.concat ", " s_constraints) :: acc)
										else
											acc;
									| _ -> acc
							) [] cl_params in
					(params, String.concat " " params_extends)
				| _ -> ("","")
		in

		let gen_field_decl w visibility v_n modifiers t n =
			let parts = ref [] in
			if visibility <> "" then parts := visibility :: !parts;
			if v_n <> "" then parts := v_n :: !parts;
			if modifiers <> [] then parts := modifiers @ !parts;
			if t <> "" then parts := t :: !parts;
			parts := n :: !parts;
			write w (String.concat " " (List.rev !parts));
		in

		let rec gen_event w is_static cl (event,t,custom,add,remove) =
			let is_interface = cl.cl_interface in
			let visibility = if is_interface then "" else "public" in
			let visibility, modifiers = get_fun_modifiers event.cf_meta visibility ["event"] in
			let v_n = if is_static then "static" else "" in
			gen_field_decl w visibility v_n modifiers (t_s (run_follow gen t)) (change_field event.cf_name);
			if custom && not is_interface then begin
				write w " ";
				begin_block w;
				print w "add { _add_%s(value); }" event.cf_name;
				newline w;
				print w "remove { _remove_%s(value); }" event.cf_name;
				newline w;
				end_block w;
				newline w;
			end else
				write w ";\n";
			newline w;
		in

		let rec gen_prop w is_static cl is_final (prop,t,get,set) =
			gen_attributes w prop.cf_meta;
			let is_interface = cl.cl_interface in
			let fn_is_final = function
				| None -> true
				| Some ({ cf_kind = Method mkind } as m) ->
					(match mkind with | MethInline -> true | _ -> false) || Meta.has Meta.Final m.cf_meta
				| _ -> assert false
			in
			let is_virtual = not (is_interface || is_final || Meta.has Meta.Final prop.cf_meta || fn_is_final get || fn_is_final set) in

			let fn_is_override = function
				| Some cf -> List.memq cf cl.cl_overrides
				| None -> false
			in
			let is_override = fn_is_override get || fn_is_override set in
			let visibility = if is_interface then "" else "public" in
			let visibility, modifiers = get_fun_modifiers prop.cf_meta visibility [] in
			let v_n = if is_static then "static" else if is_override && not is_interface then "override" else if is_virtual then "virtual" else "" in
			gen_nocompletion w prop.cf_meta;

			gen_field_decl w visibility v_n modifiers (t_s (run_follow gen t)) (change_field prop.cf_name);

			let check cf = match cf with
				| Some ({ cf_overloads = o :: _ } as cf) ->
						gen.gcon.error "Property functions with more than one overload is currently unsupported" cf.cf_pos;
						gen.gcon.error "Property functions with more than one overload is currently unsupported" o.cf_pos
				| _ -> ()
			in
			check get;
			check set;

			write w " ";
			if is_interface then begin
				write w "{ ";
				let s = ref "" in
				(match prop.cf_kind with Var { v_read = AccCall } -> write w "get;"; s := " "; | _ -> ());
				(match prop.cf_kind with Var { v_write = AccCall } -> print w "%sset;" !s | _ -> ());
				write w " }";
				newline w;
			end else begin
				begin_block w;
				(match get with
					| Some cf ->
						print w "get { return _get_%s(); }" prop.cf_name;
						newline w;
						cf.cf_meta <- (Meta.Custom "?prop_impl", [], null_pos) :: cf.cf_meta;
					| None -> ());
				(match set with
					| Some cf ->
						print w "set { _set_%s(value); }" prop.cf_name;
						newline w;
						cf.cf_meta <- (Meta.Custom "?prop_impl", [], null_pos) :: cf.cf_meta;
					| None -> ());
				end_block w;
				newline w;
				newline w;
			end;
		in

		let needs_unchecked e =
			let rec loop e = match e.eexpr with
			(* a non-zero integer constant means that we want unchecked context *)
			| TConst (TInt i) when i <> Int32.zero ->
				raise Exit

			(* don't recurse into explicit checked blocks *)
			| TCall ({ eexpr = TIdent "__checked__" }, _) ->
				()

			(* skip reflection field hashes as they are safe *)
			| TNew ({ cl_path = (["haxe"; "lang"],"DynamicObject") }, [], [_; e1; _; e2]) ->
				loop e1;
				loop e2
			| TNew ({ cl_path = (["haxe"; "lang"],"Closure") }, [], [eo; _; _]) ->
				loop eo
			| TCall ({ eexpr = TField (_, FStatic ({ cl_path = ["haxe"; "lang"],"Runtime" },
					{ cf_name = "getField" | "setField" | "getField_f" | "setField_f" | "callField" })) },
					eo :: _ :: _ :: rest) ->
				loop eo;
				List.iter loop rest

			| _ ->
				Type.iter loop e
			in
			try (loop e; false) with Exit -> true
		in

		let rec gen_class_field w ?(is_overload=false) is_static cl is_final cf =
			gen_attributes w cf.cf_meta;
			let is_interface = cl.cl_interface in
			let name, is_new, is_explicit_iface = match cf.cf_name with
				| "new" -> snd cl.cl_path, true, false
				| name when String.contains name '.' ->
					let fn_name, path = parse_explicit_iface name in
					(s_type_path path) ^ "." ^ fn_name, false, true
				| name -> try
					let binop = PMap.find name binops_names in
					"operator " ^ s_binop binop, false, false
				with | Not_found -> try
					let unop = PMap.find name unops_names in
					"operator " ^ s_unop unop, false, false
				with | Not_found ->
					if Meta.has (Meta.Custom "?prop_impl") cf.cf_meta || Meta.has (Meta.Custom ":cs_event_impl") cf.cf_meta then
						"_" ^ name, false, false
					else
						name, false, false
			in
			let rec loop_static cl =
				match is_static, cl.cl_super with
					| false, _ -> []
					| true, None -> []
					| true, Some(cl,_) ->
						(try
								let cf2 = PMap.find cf.cf_name cl.cl_statics in
								CastDetect.type_eq gen EqStrict cf.cf_type cf2.cf_type;
								["new"]
							with
								| Not_found | Unify_error _ ->
										loop_static cl
							)
			in
			let modf = loop_static cl in

			(match cf.cf_kind with
				| Var _
				| Method (MethDynamic) when Type.is_physical_field cf ->
					(if is_overload || List.exists (fun cf -> cf.cf_expr <> None) cf.cf_overloads then
						gen.gcon.error "Only normal (non-dynamic) methods can be overloaded" cf.cf_pos);
					if not is_interface then begin
						let access, modifiers = get_fun_modifiers cf.cf_meta "public" [] in
						let modifiers = modifiers @ modf in
						gen_nocompletion w cf.cf_meta;
						gen_field_decl w access (if is_static then "static" else "") modifiers (t_s (run_follow gen cf.cf_type)) (change_field name);
						(match cf.cf_expr with
							| Some e ->
								write w " = ";
								expr_s w e;
							| None -> ()
						);
						write w ";"
					end (* TODO see how (get,set) variable handle when they are interfaces *)
				| Method _ when not (Type.is_physical_field cf) || (match cl.cl_kind, cf.cf_expr with | KAbstractImpl _, None -> true | _ -> false) ->
					List.iter (fun cf -> if cl.cl_interface || cf.cf_expr <> None then
						gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
					) cf.cf_overloads
				| Var _ | Method MethDynamic -> ()
				| Method _ when is_new && Meta.has Meta.Struct cl.cl_meta && fst (get_fun cf.cf_type) = [] ->
						(* make sure that the method is empty *)
						let rec check_empty expr = match expr.eexpr with
							| TBlock(bl) -> bl = [] || List.for_all check_empty bl
							| TMeta(_,e) -> check_empty e
							| TParenthesis(e) -> check_empty e
							| TConst(TNull) -> true
							| TFunction(tf) -> check_empty tf.tf_expr
							| _ -> false
						in
						(match cf.cf_expr with
							| Some e ->
								if not (check_empty e) then
									gen.gcon.error "The body of a zero argument constructor of a struct should be empty" e.epos
							| _ -> ());
						List.iter (fun cf ->
							if cl.cl_interface || cf.cf_expr <> None then
								gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
						) cf.cf_overloads;
				| Method mkind ->
					List.iter (fun cf ->
						if cl.cl_interface || cf.cf_expr <> None then
							gen_class_field w ~is_overload:true is_static cl (Meta.has Meta.Final cf.cf_meta) cf
					) cf.cf_overloads;
					let is_virtual = not is_final && match mkind with | MethInline -> false | _ when not is_new -> true | _ -> false in
					let is_virtual = if not is_virtual || Meta.has Meta.Final cf.cf_meta then false else is_virtual in
					let is_override = List.memq cf cl.cl_overrides in
					let is_override = is_override || match cf.cf_name, follow cf.cf_type with
						| "Equals", TFun([_,_,targ], tret) ->
							(match follow targ, follow tret with
								| TDynamic _, TAbstract({ a_path = ([], "Bool") }, []) -> true
								| _ -> false)
						| "GetHashCode", TFun([],_) -> true
						| _ -> false
					in
					let is_override = if Meta.has (Meta.Custom "?prop_impl") cf.cf_meta then false else is_override in

					let is_virtual = is_virtual && not (Meta.has Meta.Final cl.cl_meta) && not (is_interface) in
					let visibility = if is_interface then "" else "public" in

					let visibility, modifiers = get_fun_modifiers cf.cf_meta visibility [] in
					let modifiers = modifiers @ modf in
					let visibility, is_virtual = if is_explicit_iface then "",false else if visibility = "private" then "private",false else visibility, is_virtual in
					let v_n = if is_static then "static" else if is_override && not is_interface then "override" else if is_virtual then "virtual" else "" in
					let cf_type = if is_override && not is_overload && not (Meta.has Meta.Overload cf.cf_meta) then match field_access gen (TInst(cl, List.map snd cl.cl_params)) cf.cf_name with | FClassField(_,_,_,_,_,actual_t,_) -> actual_t | _ -> assert false else cf.cf_type in
					let ret_type, args = match follow cf_type with | TFun (strbtl, t) -> (t, strbtl) | _ -> assert false in
					gen_nocompletion w cf.cf_meta;

					(* public static void funcName *)
					gen_field_decl w visibility v_n modifiers (if not is_new then (rett_s (run_follow gen ret_type)) else "") (change_field name);

					let params, params_ext = get_string_params cl cf.cf_params in
					(* <T>(string arg1, object arg2) with T : object *)
					(match cf.cf_expr with
					| Some { eexpr = TFunction tf } ->
							print w "%s(%s)%s" (params) (String.concat ", " (List.map2 (fun (var, _) (_,_,t) -> sprintf "%s %s" (argt_s t) (change_id var.v_name)) tf.tf_args args)) (params_ext)
					| _ ->
							print w "%s(%s)%s" (params) (String.concat ", " (List.map (fun (name, _, t) -> sprintf "%s %s" (argt_s t) (change_id name)) args)) (params_ext)
					);
					if is_interface then
						write w ";"
					else begin
						write w " ";
						let rec loop meta =
							match meta with
								| [] ->
									let expr = match cf.cf_expr with
										| None -> mk (TBlock([])) t_dynamic null_pos
										| Some s ->
											match s.eexpr with
												| TFunction tf ->
													mk_block (tf.tf_expr)
												| _ -> assert false (* FIXME *)
									in

									let write_method_expr e =
										match e.eexpr with
										| TBlock [] ->
											begin_block w;
											end_block w
										| TBlock _ ->
											let unchecked = needs_unchecked e in
											if unchecked then (begin_block w; write w "unchecked ");
											let t = Common.timer ["expression to string"] in
											expr_s w e;
											t();
											line_reset_directive w;
											if unchecked then end_block w
										| _ ->
											assert false
									in

									(if is_new then begin
										let rec get_super_call el =
											match el with
												| ( { eexpr = TCall( { eexpr = TConst(TSuper) }, _) } as call) :: rest ->
													Some call, rest
												| ( { eexpr = TBlock(bl) } as block ) :: rest ->
													let ret, mapped = get_super_call bl in
													ret, ( { block with eexpr = TBlock(mapped) } :: rest )
												| _ ->
													None, el
										in
										match expr.eexpr with
											| TBlock(bl) ->
												let super_call, rest = get_super_call bl in
												(match super_call with
													| None -> ()
													| Some sc ->
														write w ": ";
														let t = Common.timer ["expression to string"] in
														expr_s w sc;
														write w " ";
														t()
												);
												write_method_expr { expr with eexpr = TBlock(rest) }
											| _ -> assert false
									end else
										write_method_expr expr
									)
								| (Meta.FunctionCode, [Ast.EConst (Ast.String (contents,_)),_],_) :: tl ->
									begin_block w;
									write w contents;
									end_block w
								| _ :: tl -> loop tl
						in
						loop cf.cf_meta

					end);
				newline w;
				newline w;
		in

		let check_special_behaviors w cl = match cl.cl_kind with
		| KAbstractImpl _ -> ()
		| _ ->
			(* get/set pairs *)
			let pairs = ref PMap.empty in
			(try
				let get = PMap.find "__get" cl.cl_fields in
				List.iter (fun cf ->
					let args,ret = get_fun cf.cf_type in
					match args with
					| [_,_,idx] -> pairs := PMap.add (t_s idx) ( t_s ret, Some cf, None ) !pairs
					| _ -> gen.gcon.warning "The __get function must have exactly one argument (the index)" cf.cf_pos
				) (get :: get.cf_overloads)
			with | Not_found -> ());
			(try
				let set = PMap.find "__set" cl.cl_fields in
				List.iter (fun cf ->
					let args, ret = get_fun cf.cf_type in
					match args with
					| [_,_,idx; _,_,v] -> (try
						let vt, g, _ = PMap.find (t_s idx) !pairs in
						let tvt = t_s v in
						if vt <> tvt then gen.gcon.warning "The __get function of same index has a different type from this __set function" cf.cf_pos;
						pairs := PMap.add (t_s idx) (vt, g, Some cf) !pairs
					with | Not_found ->
						pairs := PMap.add (t_s idx) (t_s v, None, Some cf) !pairs)
					| _ ->
						gen.gcon.warning "The __set function must have exactly two arguments (index, value)" cf.cf_pos
				) (set :: set.cf_overloads)
			with | Not_found -> ());
			PMap.iter (fun idx (v, get, set) ->
				print w "public %s this[%s index]" v idx;
					begin_block w;
					(match get with
					| None -> ()
					| Some _ ->
						write w "get";
						begin_block w;
						write w "return this.__get(index);";
						end_block w);
					(match set with
					| None -> ()
					| Some _ ->
						write w "set";
						begin_block w;
						write w "this.__set(index,value);";
						end_block w);
					end_block w) !pairs;
			(if not (PMap.is_empty !pairs) then try
				let get = PMap.find "__get" cl.cl_fields in
				let idx_t, v_t = match follow get.cf_type with
					| TFun([_,_,arg_t],ret_t) ->
						t_s (run_follow gen arg_t), t_s (run_follow gen ret_t)
					| _ -> gen.gcon.error "The __get function must be a function with one argument. " get.cf_pos; assert false
				in
				List.iter (fun (cl,args) ->
					match cl.cl_array_access with
						| None -> ()
						| Some t ->
							let changed_t = apply_params cl.cl_params (List.map (fun _ -> t_dynamic) cl.cl_params) t in
							let t_as_s = t_s (run_follow gen changed_t) in
							print w "%s %s.this[int key]" t_as_s (t_s (TInst(cl, args)));
								begin_block w;
								write w "get";
								begin_block w;
									print w "return ((%s) this.__get(key));" t_as_s;
								end_block w;
								write w "set";
								begin_block w;
									print w "this.__set(key, (%s) value);" v_t;
								end_block w;
							end_block w;
							newline w;
							newline w
				) cl.cl_implements
			with | Not_found -> ());
			if cl.cl_interface && is_hxgen (TClassDecl cl) && is_some cl.cl_array_access then begin
				let changed_t = apply_params cl.cl_params (List.map (fun _ -> t_dynamic) cl.cl_params) (get cl.cl_array_access) in
				print w "%s this[int key]" (t_s (run_follow gen changed_t));
				begin_block w;
					write w "get;";
					newline w;
					write w "set;";
					newline w;
				end_block w;
				newline w;
				newline w
			end;
			(try
				if cl.cl_interface then raise Not_found;
				let cf = PMap.find "toString" cl.cl_fields in
				(if List.exists (fun c -> c.cf_name = "toString") cl.cl_overrides then raise Not_found);
				(match cf.cf_type with
					| TFun([], ret) ->
						(match real_type ret with
							| TInst( { cl_path = ([], "String") }, []) ->
								write w "public override string ToString()";
								begin_block w;
								write w "return this.toString();";
								end_block w;
								newline w;
								newline w
							| _ ->
								gen.gcon.error "A toString() function should return a String!" cf.cf_pos
						)
					| _ -> ()
				)
			with | Not_found -> ());
			(try
				if cl.cl_interface then raise Not_found;
				let cf = PMap.find "finalize" cl.cl_fields in
				(if List.exists (fun c -> c.cf_name = "finalize") cl.cl_overrides then raise Not_found);
				(match cf.cf_type with
					| TFun([], ret) ->
						(match real_type ret with
							| TAbstract( { a_path = ([], "Void") }, []) ->
								write w "~";
								write w (snd cl.cl_path);
								write w "()";
								begin_block w;
								write w "this.finalize();";
								end_block w;
								newline w;
								newline w
							| _ ->
								gen.gcon.error "A finalize() function should be Void->Void!" cf.cf_pos
						)
					| _ -> ()
				)
			with | Not_found -> ());
			(* properties *)
			let handle_prop static f =
				match f.cf_kind with
				| Method _ -> ()
				| Var v when Type.is_physical_field f -> ()
				| Var v ->
					let prop acc = match acc with
						| AccNo | AccNever | AccCall -> true
						| _ -> false
					in
					if prop v.v_read && prop v.v_write && (v.v_read = AccCall || v.v_write = AccCall) then begin
						let this = if static then
							ExprBuilder.make_static_this cl f.cf_pos
						else
							{ eexpr = TConst TThis; etype = TInst(cl,List.map snd cl.cl_params); epos = f.cf_pos }
						in
						print w "public %s%s %s" (if static then "static " else "") (t_s f.cf_type) (Dotnet.netname_to_hx f.cf_name);
						begin_block w;
						(match v.v_read with
						| AccCall ->
							write w "get";
							begin_block w;
							write w "return ";
							expr_s w this;
							print w ".get_%s();" f.cf_name;
							end_block w
						| _ -> ());
						(match v.v_write with
						| AccCall ->
							write w "set";
							begin_block w;
							expr_s w this;
							print w ".set_%s(value);" f.cf_name;
							end_block w
						| _ -> ());
						end_block w;
					end
			in
			if Meta.has Meta.BridgeProperties cl.cl_meta then begin
				List.iter (handle_prop true) cl.cl_ordered_statics;
				List.iter (handle_prop false) cl.cl_ordered_fields;
			end
		in

		let gen_class w cl =
			write w "#pragma warning disable 109, 114, 219, 429, 168, 162";
			newline w;
			let should_close = match change_ns (TClassDecl cl) (fst (cl.cl_path)) with
				| [] -> false
				| ns ->
					print w "namespace %s " (String.concat "." ns);
					begin_block w;
					true
			in

			(try
				let _,m,_ = Meta.get (Meta.Custom "generic_iface") cl.cl_meta in
				let rec loop i acc =
					if i == 0 then
						acc
					else
						"object" :: (loop (pred i) acc)
				in
				let tparams = loop (match m with [(EConst(Int s),_)] -> int_of_string s | _ -> assert false) [] in
				cl.cl_meta <- (Meta.Meta, [
					EConst(String("global::haxe.lang.GenericInterface(typeof(global::" ^ module_s (TClassDecl cl) ^ "<" ^ String.concat ", " tparams ^ ">))",Double) ), cl.cl_pos
				], cl.cl_pos) :: cl.cl_meta
			with Not_found ->
				());

			gen_attributes w cl.cl_meta;

			let is_main =
				match gen.gcon.main_class with
					| Some ( (_,"Main") as path) when path = cl.cl_path && not cl.cl_interface ->
						(*
							for cases where the main class is called Main, there will be a problem with creating the entry point there.
							In this special case, a special entry point class will be created
						*)
						write w "public class EntryPoint__Main ";
						begin_block w;
						write w "public static void Main() ";
						begin_block w;
						(if Hashtbl.mem gen.gtypes (["cs"], "Boot") then write w "global::cs.Boot.init();"; newline w);
						(match gen.gcon.main with
							| None ->
								expr_s w { eexpr = TTypeExpr(TClassDecl cl); etype = t_dynamic; epos = null_pos };
								write w ".main();"
							| Some expr ->
								expr_s w (mk_block expr));
						end_block w;
						end_block w;
						newline w;
						false
					| Some path when path = cl.cl_path && not cl.cl_interface -> true
					| _ -> false
			in

			let clt, access, modifiers = get_class_modifiers cl.cl_meta (if cl.cl_interface then "interface" else "class") "public" [] in
			let is_final = clt = "struct" || Meta.has Meta.Final cl.cl_meta in

			let modifiers = [access] @ modifiers in
			print w "%s %s %s" (String.concat " " modifiers) clt (change_clname (snd cl.cl_path));
			(* type parameters *)
			let params, params_ext = get_string_params cl cl.cl_params in
			let extends_implements = (match cl.cl_super with | None -> [] | Some (cl,p) -> [path_param_s (TClassDecl cl) cl.cl_path p]) @ (List.map (fun (cl,p) -> path_param_s (TClassDecl cl) cl.cl_path p) cl.cl_implements) in
			(match extends_implements with
				| [] -> print w "%s%s " params params_ext
				| _ -> print w "%s : %s%s " params (String.concat ", " extends_implements) params_ext);
			(* class head ok: *)
			(* public class Test<A> : X, Y, Z where A : Y *)
			begin_block w;
			newline w;
			(* our constructor is expected to be a normal "new" function *
			if !strict_mode && is_some cl.cl_constructor then assert false;*)

			let rec loop meta =
				match meta with
					| [] -> ()
					| (Meta.ClassCode, [Ast.EConst (Ast.String (contents,_)),_],_) :: tl ->
						write w contents
					| _ :: tl -> loop tl
			in
			loop cl.cl_meta;

			if is_main then begin
				write w "public static void Main()";
				begin_block w;
				(if Hashtbl.mem gen.gtypes (["cs"], "Boot") then write w "global::cs.Boot.init();"; newline w);
				(match gen.gcon.main with
					| None ->
						write w "main();";
					| Some expr ->
							expr_s w (mk_block expr));
				end_block w
			end;

			(match cl.cl_init with
				| None -> ()
				| Some init ->
					print w "static %s() " (snd cl.cl_path);
					if needs_unchecked init then begin
						begin_block w;
						write w "unchecked ";
						expr_s w (mk_block init);
						end_block w;
					end else
						expr_s w (mk_block init);
					line_reset_directive w;
					newline w;
					newline w
			);

			(* collect properties and events *)
			let partition cf cflist =
				let events, props, nonprops = ref [], ref [], ref [] in

				List.iter (fun v -> match v.cf_kind with
					| Var { v_read = AccCall } | Var { v_write = AccCall } when not (Type.is_physical_field v) && Meta.has Meta.Property v.cf_meta ->
						props := (v.cf_name, ref (v, v.cf_type, None, None)) :: !props;
					| Var { v_read = AccNormal; v_write = AccNormal } when Meta.has Meta.Event v.cf_meta ->
						events := (v.cf_name, ref (v, v.cf_type, false, None, None)) :: !events;
					| _ ->
						nonprops := v :: !nonprops;
				) cflist;

				let events, nonprops = !events, !nonprops in

				let t = TInst(cl, List.map snd cl.cl_params) in
				let find_prop name = try
						List.assoc name !props
					with | Not_found -> match field_access gen t name with
						| FClassField (_,_,decl,v,_,t,_) when is_extern_prop (TInst(cl,List.map snd cl.cl_params)) name ->
							let ret = ref (v,t,None,None) in
							props := (name, ret) :: !props;
							ret
						| _ -> raise Not_found
				in

				let find_event name = List.assoc name events in

				let is_empty_function cf = match cf.cf_expr with
					| Some {eexpr = TFunction { tf_expr = {eexpr = TBlock []}}} -> true
					| _ -> false
				in

				let interf = cl.cl_interface in
				(* get all functions that are getters/setters *)
				let nonprops = List.filter (function
					| cf when String.starts_with cf.cf_name "get_" -> (try
						(* find the property *)
						let prop = find_prop (String.sub cf.cf_name 4 (String.length cf.cf_name - 4)) in
						let v, t, get, set = !prop in
						assert (get = None);
						prop := (v,t,Some cf,set);
						not interf
					with | Not_found -> true)
					| cf when String.starts_with cf.cf_name "set_" -> (try
						(* find the property *)
						let prop = find_prop (String.sub cf.cf_name 4 (String.length cf.cf_name - 4)) in
						let v, t, get, set = !prop in
						assert (set = None);
						prop := (v,t,get,Some cf);
						not interf
					with | Not_found -> true)
					| cf when String.starts_with cf.cf_name "add_" -> (try
						let event = find_event (String.sub cf.cf_name 4 (String.length cf.cf_name - 4)) in
						let v, t, _, add, remove = !event in
						assert (add = None);
						let custom = not (is_empty_function cf) in
						event := (v, t, custom, Some cf, remove);
						false
					with | Not_found -> true)
					| cf when String.starts_with cf.cf_name "remove_" -> (try
						let event = find_event (String.sub cf.cf_name 7 (String.length cf.cf_name - 7)) in
						let v, t, _, add, remove = !event in
						assert (remove = None);
						let custom = not (is_empty_function cf) in
						event := (v, t, custom, add, Some cf);
						false
					with | Not_found -> true)
					| _ -> true
				) nonprops in

				let nonprops = ref nonprops in
				List.iter (fun (n,r) ->
					let ev, t, custom, add, remove = !r in
					match add, remove with
					| Some add, Some remove ->
						if custom && not cl.cl_interface then
							nonprops := add :: remove :: !nonprops
					| _ -> assert false (* shouldn't happen because Filters.check_cs_events makes sure methods are present *)
				) events;

				let evts = List.map (fun(_,v) -> !v) events in
				let ret = List.map (fun (_,v) -> !v) !props in
				let ret = List.filter (function | (_,_,None,None) -> false | _ -> true) ret in
				evts, ret, List.rev !nonprops
			in

			let fevents, fprops, fnonprops = partition cl cl.cl_ordered_fields in
			let sevents, sprops, snonprops = partition cl cl.cl_ordered_statics in
			(if is_some cl.cl_constructor then gen_class_field w false cl is_final (get cl.cl_constructor));
			if not cl.cl_interface then begin
				(* we don't want to generate properties for abstract implementation classes, because they don't have object to work with *)
				List.iter (gen_event w true cl) sevents;
				if (match cl.cl_kind with KAbstractImpl _ -> false | _ -> true) then List.iter (gen_prop w true cl is_final) sprops;
				List.iter (gen_class_field w true cl is_final) snonprops
			end;
			List.iter (gen_event w false cl) fevents;
			List.iter (gen_prop w false cl is_final) fprops;
			List.iter (gen_class_field w false cl is_final) fnonprops;
			check_special_behaviors w cl;
			end_block w;
			if cl.cl_interface && cl.cl_ordered_statics <> [] then begin
				print w "public class %s__Statics_" (snd cl.cl_path);
				begin_block w;
				List.iter (gen_class_field w true { cl with cl_interface = false } is_final) cl.cl_ordered_statics;
				end_block w
			end;
			if should_close then end_block w
		in


		let gen_enum w e =
			let should_close = match change_ns (TEnumDecl e) (fst e.e_path) with
				| [] -> false
				| ns ->
					print w "namespace %s" (String.concat "." ns);
					begin_block w;
					true
			in
			gen_attributes w e.e_meta;

			print w "public enum %s" (change_clname (snd e.e_path));
			begin_block w;
			write w (String.concat ", " (List.map (change_id) e.e_names));
			end_block w;

			if should_close then end_block w
		in

		let module_type_gen w md_tp =
			let file_start = len w = 0 in
			let requires_root = no_root && file_start in
			if file_start then
				Codegen.map_source_header gen.gcon (fun s -> print w "// %s\n" s);
			reset_temps();
			match md_tp with
				| TClassDecl cl ->
					if not cl.cl_extern then begin
						(if requires_root then write w "using haxe.root;\n"; newline w;);
						gen_class w cl;
						newline w;
						newline w
					end;
					(not cl.cl_extern)
				| TEnumDecl e ->
					if not e.e_extern && not (Meta.has Meta.Class e.e_meta) then begin
						(if requires_root then write w "using haxe.root;\n"; newline w;);
						gen_enum w e;
						newline w;
						newline w
					end;
					(not e.e_extern)
				| TAbstractDecl _
				| TTypeDecl _ ->
					false
		in

		(* generate source code *)
		init_ctx gen;

		Hashtbl.add gen.gspecial_vars "__rethrow__" true;
		Hashtbl.add gen.gspecial_vars "__typeof__" true;
		Hashtbl.add gen.gspecial_vars "__is__" true;
		Hashtbl.add gen.gspecial_vars "__as__" true;
		Hashtbl.add gen.gspecial_vars "__cs__" true;

		Hashtbl.add gen.gspecial_vars "__checked__" true;
		Hashtbl.add gen.gspecial_vars "__lock__" true;
		Hashtbl.add gen.gspecial_vars "__fixed__" true;
		Hashtbl.add gen.gspecial_vars "__unsafe__" true;
		Hashtbl.add gen.gspecial_vars "__addressOf__" true;
		Hashtbl.add gen.gspecial_vars "__valueOf__" true;
		Hashtbl.add gen.gspecial_vars "__sizeof__" true;
		Hashtbl.add gen.gspecial_vars "__stackalloc__" true;

		Hashtbl.add gen.gspecial_vars "__delegate__" true;
		Hashtbl.add gen.gspecial_vars "__array__" true;
		Hashtbl.add gen.gspecial_vars "__ptr__" true;

		Hashtbl.add gen.gsupported_conversions (["haxe"; "lang"], "Null") (fun t1 t2 -> true);
		let last_needs_box = gen.gneeds_box in
		gen.gneeds_box <- (fun t -> match (gen.greal_type t) with
			| TAbstract( ( { a_path = ["cs"], "Pointer" }, _ ) )
			| TInst( { cl_path = ["cs"], "Pointer" }, _ )
			| TInst( { cl_path = (["haxe"; "lang"], "Null") }, _ ) -> true
			| _ -> last_needs_box t);

		gen.greal_type <- real_type;
		gen.greal_type_param <- change_param_type;

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
						cf.cf_expr <- Option.map type_map cf.cf_expr;

						(* add @:skipReflection to @:event vars *)
						match cf.cf_kind with
						| Var _ when (Meta.has Meta.Event cf.cf_meta) && not (Meta.has Meta.SkipReflection cf.cf_meta) ->
							cf.cf_meta <- (Meta.SkipReflection, [], null_pos) :: cf.cf_meta;
						| _ -> ()

					) all_fields;
				cl.cl_dynamic <- Option.map run_follow_gen cl.cl_dynamic;
				cl.cl_array_access <- Option.map run_follow_gen cl.cl_array_access;
				cl.cl_init <- Option.map type_map cl.cl_init;
				cl.cl_super <- Option.map super_map cl.cl_super;
				cl.cl_implements <- List.map super_map cl.cl_implements
			| _ -> ()
			) gen.gtypes_list;

		let mk_tp t pos = { eexpr = TIdent "$type_param"; etype = t; epos = pos } in
		gen.gparam_func_call <- (fun ecall efield params elist ->
			match efield.eexpr with
			| TField(_, FEnum _) ->
				{ ecall with eexpr = TCall(efield, elist) }
			| _ ->
				{ ecall with eexpr = TCall(efield, (List.map (fun t -> mk_tp t ecall.epos) params) @ elist) }
		);

		if not erase_generics then HardNullableSynf.configure gen
			(fun e ->
				match e.eexpr, real_type e.etype with
					| TConst TThis, _ when gen.gcurrent_path = (["haxe";"lang"], "Null") ->
						e
					| _, TInst({ cl_path = (["haxe";"lang"], "Null") }, [t]) ->
						let e = { e with eexpr = TParenthesis(e) } in
						{ (mk_field_access gen e "value" e.epos) with etype = t }
					| _ ->
						trace (debug_type e.etype); gen.gcon.error "This expression is not a Nullable expression" e.epos; assert false
			)
			(fun v t has_value ->
				match has_value, real_type v.etype with
					| true, TDynamic _ | true, TAnon _ | true, TMono _ ->
						{
							eexpr = TCall(mk_static_field_access_infer null_t "ofDynamic" v.epos [t], [mk_tp t v.epos; v]);
							etype = TInst(null_t, [t]);
							epos = v.epos
						}
					| _ ->
						{ eexpr = TNew(null_t, [t], [gen.ghandle_cast t v.etype v; { eexpr = TConst(TBool has_value); etype = gen.gcon.basic.tbool; epos = v.epos } ]); etype = TInst(null_t, [t]); epos = v.epos }
			)
			(fun e ->
				{
					eexpr = TCall(
						{ (mk_field_access gen { (mk_paren e) with etype = real_type e.etype } "toDynamic" e.epos) with etype = TFun([], t_dynamic) },
						[]);
					etype = t_dynamic;
					epos = e.epos
				}
			)
			(fun e ->
				mk_field_access gen { e with etype = real_type e.etype } "hasValue" e.epos
			)
			(fun e1 e2 ->
				mk (TCall(mk_field_access gen e1 "Equals" e1.epos, [e2])) basic.tbool e1.epos
			);


		let explicit_fn_name c tl fname =
			path_param_s (TClassDecl c) c.cl_path tl ^ "." ^ fname
		in

		FixOverrides.configure ~explicit_fn_name:explicit_fn_name ~get_vmtype:real_type gen;

		let allowed_meta = Hashtbl.create 1 in
		Hashtbl.add allowed_meta Meta.LoopLabel true;
		Normalize.configure gen ~allowed_metas:allowed_meta;

		AbstractImplementationFix.configure gen;

		let cl_arg_exc = get_cl (get_type gen (["System"],"ArgumentException")) in
		let cl_arg_exc_t = TInst (cl_arg_exc, []) in
		let mk_arg_exception msg pos = mk (TNew (cl_arg_exc, [], [ExprBuilder.make_string gen.gcon msg pos])) cl_arg_exc_t pos in
		let closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx gen (get_cl (get_type gen (["haxe";"lang"],"Function"))) 6 mk_arg_exception in
		ClosuresToClass.configure gen closure_t;

		let enum_base = (get_cl (get_type gen (["haxe";"lang"],"Enum")) ) in
		let type_enumindex = mk_static_field_access_infer gen.gclasses.cl_type "enumIndex" null_pos [] in
		let mk_enum_index_call e p =
			mk (TCall (type_enumindex, [e])) gen.gcon.basic.tint p
		in
		EnumToClass2.configure gen enum_base mk_enum_index_call;

		InterfaceVarsDeleteModf.configure gen;

		let dynamic_object = (get_cl (get_type gen (["haxe";"lang"],"DynamicObject")) ) in

		let object_iface = get_cl (get_type gen (["haxe";"lang"],"IHxObject")) in

		let empty_en = match get_type gen (["haxe";"lang"], "EmptyObject") with TEnumDecl e -> e | _ -> assert false in
		let empty_ctor_type = TEnum(empty_en, []) in
		let empty_en_expr = mk (TTypeExpr (TEnumDecl empty_en)) (TAnon { a_fields = PMap.empty; a_status = ref (EnumStatics empty_en) }) null_pos in
		let empty_ctor_expr = mk (TField (empty_en_expr, FEnum(empty_en, PMap.find "EMPTY" empty_en.e_constrs))) empty_ctor_type null_pos in
		OverloadingConstructor.configure ~empty_ctor_type:empty_ctor_type ~empty_ctor_expr:empty_ctor_expr gen;

		let rcf_static_find = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "findHash" null_pos [] in
		let rcf_static_lookup = mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "lookupHash" null_pos [] in

		let rcf_static_insert, rcf_static_remove =
			if erase_generics then begin
				let get_specialized_postfix t = match t with
					| TAbstract({a_path = [],("Float" | "Int" as name)}, _) -> name
					| TAnon _ | TDynamic _ -> "Dynamic"
					| _ -> print_endline (debug_type t); assert false
				in
				(fun t -> mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) ("insert" ^ get_specialized_postfix t) null_pos []),
				(fun t -> mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) ("remove" ^ get_specialized_postfix t) null_pos [])
			end else
				(fun t -> mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "insert" null_pos [t]),
				(fun t -> mk_static_field_access_infer (get_cl (get_type gen (["haxe";"lang"], "FieldLookup"))) "remove" null_pos [t])
		in

		let can_be_float = like_float in

		let rcf_on_getset_field main_expr field_expr field may_hash may_set is_unsafe =
			let is_float = can_be_float (real_type main_expr.etype) in
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
				mk_nativearray_decl gen t_dynamic args ecall.epos
			else
				null (gen.gclasses.nativearray t_dynamic) ecall.epos
			in

			let call_args =
				[field_expr; { field_expr with eexpr = TConst(TString field); etype = basic.tstring } ]
					@ hash_arg
					@ [ arr_call ]
			in

			mk_cast ecall.etype { ecall with eexpr = TCall(infer, call_args) }
		in

		add_cast_handler gen;
		if not erase_generics then
			RealTypeParams.configure gen (fun e t -> gen.gcon.warning ("Cannot cast to " ^ (debug_type t)) e.epos; mk_cast t e) ifaces (get_cl (get_type gen (["haxe";"lang"], "IGenericObject")))
		else
			RealTypeParams.RealTypeParamsModf.configure gen (RealTypeParams.RealTypeParamsModf.set_only_hxgeneric gen);

		let flookup_cl = get_cl (get_type gen (["haxe";"lang"], "FieldLookup")) in

		let cl_field_exc = get_cl (get_type gen (["System"],"MemberAccessException")) in
		let cl_field_exc_t = TInst (cl_field_exc, []) in
		let mk_field_exception msg pos = mk (TNew (cl_field_exc, [], [ExprBuilder.make_string gen.gcon msg pos])) cl_field_exc_t pos in

		let rcf_ctx =
			ReflectionCFs.new_ctx
				gen
				closure_t
				object_iface
				true
				rcf_on_getset_field
				rcf_on_call_field
				(fun hash hash_array length -> { hash with eexpr = TCall(rcf_static_find, [hash; hash_array; length]); etype=basic.tint })
				(fun hash -> { hash with eexpr = TCall(rcf_static_lookup, [hash]); etype = gen.gcon.basic.tstring })
				(fun hash_array length pos value ->
					let ecall = mk (TCall(rcf_static_insert value.etype, [hash_array; length; pos; value])) (if erase_generics then hash_array.etype else basic.tvoid) hash_array.epos in
					if erase_generics then { ecall with eexpr = TBinop(OpAssign, hash_array, ecall) } else ecall
				)
				(fun hash_array length pos ->
					let t = gen.gclasses.nativearray_type hash_array.etype in
					{ hash_array with eexpr = TCall(rcf_static_remove t, [hash_array; length; pos]); etype = gen.gcon.basic.tvoid }
				)
				(
					let delete = mk_static_field_access_infer flookup_cl "deleteHashConflict" null_pos [] in
					let get = mk_static_field_access_infer flookup_cl "getHashConflict" null_pos [] in
					let set = mk_static_field_access_infer flookup_cl "setHashConflict" null_pos [] in
					let add = mk_static_field_access_infer flookup_cl "addHashConflictNames" null_pos [] in
					let conflict_t = TInst (get_cl (get_type gen (["haxe"; "lang"], "FieldHashConflict")), []) in
					Some {
						t = conflict_t;
						get_conflict = (fun ehead ehash ename -> mk (TCall (get, [ehead; ehash; ename])) conflict_t ehead.epos);
						set = (fun ehead ehash ename evalue -> mk (TCall (set, [ehead; ehash; ename; evalue])) basic.tvoid ehead.epos);
						delete = (fun ehead ehash ename -> mk (TCall (delete, [ehead; ehash; ename])) basic.tbool ehead.epos);
						add_names = (fun ehead earr -> mk (TCall (add, [ehead; earr])) basic.tvoid ehead.epos);
					}
				)
				mk_field_exception
		in

		ReflectionCFs.UniversalBaseClass.configure gen (get_cl (get_type gen (["haxe";"lang"],"HxObject")) ) object_iface dynamic_object;

		ReflectionCFs.configure_dynamic_field_access rcf_ctx;

		let closure_cl = get_cl (get_type gen (["haxe";"lang"],"Closure")) in
		let varargs_cl = get_cl (get_type gen (["haxe";"lang"],"VarArgsFunction")) in
		let dynamic_name = mk_internal_name "hx" "invokeDynamic" in

		List.iter (fun cl ->
			List.iter (fun cf ->
				if cf.cf_name = dynamic_name then cl.cl_overrides <- cf :: cl.cl_overrides
			) cl.cl_ordered_fields
		) [closure_cl; varargs_cl];

		ReflectionCFs.implement_varargs_cl rcf_ctx ( get_cl (get_type gen (["haxe";"lang"], "VarArgsBase")) );

		let slow_invoke_field = mk_static_field_access_infer runtime_cl "slowCallField" null_pos [] in
		let slow_invoke ethis efield eargs =
			mk (TCall (slow_invoke_field, [ethis; efield; eargs])) t_dynamic ethis.epos
		in
		ReflectionCFs.configure rcf_ctx object_iface ~slow_invoke:slow_invoke;

		ObjectDeclMap.configure gen (ReflectionCFs.implement_dynamic_object_ctor rcf_ctx dynamic_object);

		InitFunction.configure gen;
		TArrayTransform.configure gen (
		fun e binop ->
			match e.eexpr with
				| TArray(e1, e2) ->
					(match follow e1.etype with
						| TDynamic _ | TAnon _ | TMono _ -> true
						| TInst({ cl_kind = KTypeParameter _ }, _) -> true
						| TInst(c,p) when erase_generics && is_hxgeneric (TClassDecl c) && is_hxgen (TClassDecl c) -> (match c.cl_path with
							| [],"String"
							| ["cs"],"NativeArray" -> false
							| _ ->
								true)
						| _ -> match binop, change_param_type (t_to_md e1.etype) [e.etype] with
							| Some(Ast.OpAssignOp _), ([TDynamic _] | [TAnon _]) ->
								true
							| _ -> false)
				| _ -> assert false
		) "__get" "__set";

		let field_is_dynamic t field =
			match field_access_esp gen (gen.greal_type t) field with
				| FEnumField _ -> false
				| FClassField (cl,p,_,_,_,t,_) ->
					if not erase_generics then
						false
					else
						let p = change_param_type (TClassDecl cl) p in
						is_dynamic (apply_params cl.cl_params p t)
				| _ -> true
		in

		let is_dynamic_expr e = is_dynamic e.etype || match e.eexpr with
			| TField(tf, f) -> field_is_dynamic tf.etype (f)
			| _ -> false
		in

		let may_nullable t = match gen.gfollow#run_f t with
			| TAbstract({ a_path = ([], "Null") }, [t]) ->
				(match follow t with
					| TInst({ cl_path = ([], "String") }, [])
					| TAbstract ({ a_path = ([], "Float") },[])
					| TInst({ cl_path = (["haxe"], "Int32")}, [] )
					| TInst({ cl_path = (["haxe"], "Int64")}, [] )
					| TAbstract ({ a_path = ([], "Int") },[])
					| TAbstract ({ a_path = ([], "Bool") },[]) -> Some t
					| TAbstract _ when like_float t -> Some t
					| t when is_cs_basic_type t -> Some t
					| _ -> None )
			| _ -> None
		in

		let is_double t = like_float t && not (like_int t) in
		let is_int t = like_int t in

		let is_null t = match real_type t with
			| TInst( { cl_path = (["haxe";"lang"], "Null") }, _ ) -> true
			| _ -> false
		in

		let is_null_expr e = is_null e.etype || match e.eexpr with
			| TField(tf, f) -> (match field_access_esp gen (real_type tf.etype) (f) with
				| FClassField(_,_,_,_,_,actual_t,_) -> is_null actual_t
				| _ -> false)
			| _ -> false
		in

		let should_handle_opeq t =
			match real_type t with
				| TDynamic _ | TAnon _ | TMono _
				| TInst( { cl_kind = KTypeParameter _ }, _ )
				| TInst( { cl_path = (["haxe";"lang"], "Null") }, _ ) -> true
				| _ -> false
		in

		let string_cl = match gen.gcon.basic.tstring with
			| TInst(c,[]) -> c
			| _ -> assert false
		in

		let is_undefined e = match e.eexpr with
			| TIdent "__undefined__" | TField(_,FStatic({cl_path=["haxe";"lang"],"Runtime"},{cf_name="undefined"})) -> true
			| _ -> false
		in

		let nullable_basic t = match gen.gfollow#run_f t with
			| TType({ t_path = ([],"Null") }, [t])
			| TAbstract({ a_path = ([],"Null") }, [t]) when is_cs_basic_type t ->
				Some(t)
			| _ ->
				None
		in

		DynamicOperators.configure gen
			~handle_strings:false
			(fun e -> match e.eexpr with
				| TBinop (Ast.OpEq, e1, e2)
				| TBinop (Ast.OpNotEq, e1, e2) ->
					(
						(* dont touch (v == null) and (null == v) comparisons because they are handled by HardNullableSynf later *)
						match e1.eexpr, e2.eexpr with
						| TConst(TNull), _ when (not (is_tparam e2.etype) && is_dynamic e2.etype) || is_null_expr e2 ->
							false
						| _, TConst(TNull) when (not (is_tparam e1.etype) && is_dynamic e1.etype) || is_null_expr e1 ->
							false
						| _ when is_undefined e1 || is_undefined e2 ->
							false
						| _ ->
							should_handle_opeq e1.etype || should_handle_opeq e2.etype
					)
				| TBinop (Ast.OpAssignOp Ast.OpAdd, e1, e2) ->
					is_dynamic_expr e1 || is_null_expr e1 || is_string e.etype
				| TBinop (Ast.OpAdd, e1, e2) -> is_dynamic e1.etype || is_dynamic e2.etype || is_tparam e1.etype || is_tparam e2.etype || is_string e1.etype || is_string e2.etype || is_string e.etype
				| TBinop (Ast.OpLt, e1, e2)
				| TBinop (Ast.OpLte, e1, e2)
				| TBinop (Ast.OpGte, e1, e2)
				| TBinop (Ast.OpGt, e1, e2) -> is_dynamic e.etype || is_dynamic e1.etype || is_dynamic e2.etype || is_string e1.etype || is_string e2.etype
				| TBinop (_, e1, e2) -> is_dynamic e.etype || is_dynamic_expr e1 || is_dynamic_expr e2
				| TUnop (_, _, e1) -> is_dynamic_expr e1 || is_null_expr e1 (* we will see if the expression is Null<T> also, as the unwrap from Unop will be the same *)
				| _ -> false)
			(fun e1 e2 ->
				let is_basic = is_cs_basic_type (follow e1.etype) || is_cs_basic_type (follow e2.etype) in
				let is_ref = if is_basic then false else match follow e1.etype, follow e2.etype with
					| TDynamic _, _
					| _, TDynamic _
					| TInst( { cl_path = ([], "String") }, [] ), _
					| _, TInst( { cl_path = ([], "String") }, [] )
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
					| _ when is_string e.etype || is_string e1.etype || is_string e2.etype ->
						{
							eexpr = TCall(
								mk_static_field_access_infer runtime_cl "concat" e.epos [],
								[ e1; e2 ]
							);
							etype = basic.tstring;
							epos = e.epos
						}
					| _ ->
						let static = mk_static_field_access_infer (runtime_cl) "plus"  e1.epos [] in
						mk_cast e.etype { eexpr = TCall(static, [e1; e2]); etype = t_dynamic; epos=e1.epos })
		(fun op e e1 e2 ->
				match nullable_basic e1.etype, nullable_basic e2.etype with
					| Some(t1), None when is_cs_basic_type e2.etype ->
						{ e with eexpr = TBinop(op, mk_cast t1 e1, e2) }
					| None, Some(t2) when is_cs_basic_type e1.etype ->
						{ e with eexpr = TBinop(op, e1, mk_cast t2 e2) }
					| _ ->
							let handler = if is_string e1.etype then begin
									{ e1 with eexpr = TCall(mk_static_field_access_infer string_cl "Compare" e1.epos [], [ e1; e2 ]); etype = gen.gcon.basic.tint }
								end else begin
									let static = mk_static_field_access_infer (runtime_cl) "compare" e1.epos [] in
									{ eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tint; epos=e1.epos }
								end
							in
							let zero = ExprBuilder.make_int gen.gcon 0 e.epos in
							{ e with eexpr = TBinop(op, handler, zero) }
		);

		FilterClosures.configure gen (fun e1 s -> true) (ReflectionCFs.get_closure_func rcf_ctx closure_cl);

		ClassInstance.configure gen;

		CastDetect.configure gen (Some empty_ctor_type) (not erase_generics) ~overloads_cast_to_base:true;

		SwitchToIf.configure gen (fun e ->
			match e.eexpr with
				| TSwitch(cond, cases, def) ->
					(match gen.gfollow#run_f cond.etype with
						| TAbstract ({ a_path = ([], "Int") },[])
						| TInst({ cl_path = ([], "String") },[]) ->
							(List.exists (fun (c,_) ->
								List.exists (fun expr -> match expr.eexpr with | TConst _ -> false | _ -> true ) c
							) cases)
						| _ -> true
					)
				| _ -> assert false
		);

		ExpressionUnwrap.configure gen;

		UnnecessaryCastsRemoval.configure gen;

		IntDivisionSynf.configure gen;

		UnreachableCodeEliminationSynf.configure gen false;

		ArrayDeclSynf.configure gen native_arr_cl change_param_type;

		CSharpSpecificSynf.configure gen runtime_cl;
		CSharpSpecificESynf.configure gen runtime_cl;

		let out_files = ref [] in

		(* copy resource files *)
		if Hashtbl.length gen.gcon.resources > 0 then begin
			let src =
					gen.gcon.file ^ "/src/Resources"
			in
			Hashtbl.iter (fun name v ->
				let name = Codegen.escape_res_name name true in
				let full_path = src ^ "/" ^ name in
				mkdir_from_path full_path;

				let f = open_out_bin full_path in
				output_string f v;
				close_out f;

				out_files := (Path.unique_full_path full_path) :: !out_files
			) gen.gcon.resources;
		end;
		(* add resources array *)
		(try
			let res = get_cl (Hashtbl.find gen.gtypes (["haxe"], "Resource")) in
			let cf = PMap.find "content" res.cl_statics in
			let res = ref [] in
			Hashtbl.iter (fun name v ->
				res := { eexpr = TConst(TString name); etype = gen.gcon.basic.tstring; epos = null_pos } :: !res;
			) gen.gcon.resources;
			cf.cf_expr <- Some ({ eexpr = TArrayDecl(!res); etype = gen.gcon.basic.tarray gen.gcon.basic.tstring; epos = null_pos })
		with | Not_found -> ());

		run_filters gen;
		(* after the filters have been run, add all hashed fields to FieldLookup *)

		let normalize_i i =
			let i = Int32.of_int (i) in
			if i < Int32.zero then
				Int32.logor (Int32.logand i (Int32.of_int 0x3FFFFFFF)) (Int32.shift_left Int32.one 30)
			else i
		in

		let nhash = ref 0 in
		let hashes = Hashtbl.fold (fun i s acc -> incr nhash; (normalize_i i,s) :: acc) rcf_ctx.rcf_hash_fields [] in
		let hashes = List.sort (fun (i,s) (i2,s2) -> compare i i2) hashes in

		let haxe_libs = List.filter (function (_,_,_,lookup) -> is_some (lookup (["haxe";"lang"], "DceNo"))) gen.gcon.net_libs in
		(try
			(* first let's see if we're adding a -net-lib that has already a haxe.lang.FieldLookup *)
			let name,_,_,_ = List.find (function (_,_,_,lookup) -> is_some (lookup (["haxe";"lang"], "FieldLookup"))) gen.gcon.net_libs in
			if not (Common.defined gen.gcon Define.DllImport) then begin
				gen.gcon.warning ("The -net-lib with path " ^ name ^ " contains a Haxe-generated assembly. Please define `-D dll_import` to handle Haxe-generated dll import correctly") null_pos;
				raise Not_found
			end;
			if not (List.exists (function (n,_,_,_) -> n = name) haxe_libs) then
				gen.gcon.warning ("The -net-lib with path " ^ name ^ " contains a Haxe-generated assembly, however it wasn't compiled with `-dce no`. Recompilation with `-dce no` is recommended") null_pos;
			(* it has; in this case, we need to add the used fields on each __init__ *)
			flookup_cl.cl_extern <- true;
			let hashs_by_path = Hashtbl.create !nhash in
			Hashtbl.iter (fun (path,i) s -> Hashtbl.add hashs_by_path path (i,s)) rcf_ctx.rcf_hash_paths;
			Hashtbl.iter (fun _ md -> match md with
				| TClassDecl ({ cl_extern = false; cl_interface = false } as c) -> (try
					let all = Hashtbl.find_all hashs_by_path c.cl_path in
					let all = List.map (fun (i,s) -> normalize_i i, s) all in
					let all = List.sort (fun (i,s) (i2,s2) -> compare i i2) all in

					if all <> [] then begin
						let add = mk_static_field_access_infer flookup_cl "addFields" c.cl_pos [] in
						let expr = { eexpr = TCall(add, [
							mk_nativearray_decl gen basic.tint (List.map (fun (i,s) -> { eexpr = TConst(TInt (i)); etype = basic.tint; epos = c.cl_pos }) all) c.cl_pos;
							mk_nativearray_decl gen basic.tstring (List.map (fun (i,s) -> { eexpr = TConst(TString (s)); etype = basic.tstring; epos = c.cl_pos }) all) c.cl_pos;
						]); etype = basic.tvoid; epos = c.cl_pos } in
						match c.cl_init with
							| None -> c.cl_init <- Some expr
							| Some e ->
								c.cl_init <- Some { eexpr = TBlock([expr;e]); etype = basic.tvoid; epos = e.epos }
					end
				with | Not_found -> ())
				| _ -> ()) gen.gtypes;

		with | Not_found -> try
			let basic = gen.gcon.basic in
			let cl = flookup_cl in
			let field_ids = PMap.find "fieldIds" cl.cl_statics in
			let fields = PMap.find "fields" cl.cl_statics in

			field_ids.cf_expr <- Some (mk_nativearray_decl gen basic.tint (List.map (fun (i,s) -> { eexpr = TConst(TInt (i)); etype = basic.tint; epos = field_ids.cf_pos }) hashes) field_ids.cf_pos);
			fields.cf_expr <- Some (mk_nativearray_decl gen basic.tstring (List.map (fun (i,s) -> { eexpr = TConst(TString s); etype = basic.tstring; epos = fields.cf_pos }) hashes) fields.cf_pos);

		with | Not_found ->
			gen.gcon.error "Fields 'fieldIds' and 'fields' were not found in class haxe.lang.FieldLookup" flookup_cl.cl_pos
		);

		if Common.defined gen.gcon Define.DllImport then begin
			Hashtbl.iter (fun _ md -> match md with
				| TClassDecl ({ cl_extern = false } as c) -> (try
					let extra = match c.cl_params with
						| _ :: _ when not erase_generics -> "_" ^ string_of_int (List.length c.cl_params)
						| _ -> ""
					in
					let pack = match c.cl_path with
						| ([], _) when no_root && is_hxgen (TClassDecl c) ->
							["haxe";"root"]
						| (p,_) -> p
					in
					let path = (pack, snd c.cl_path ^ extra) in
					ignore (List.find (function (_,_,_,lookup) ->
						is_some (lookup path)) haxe_libs);
					c.cl_extern <- true;
				with | Not_found -> ())
				| _ -> ()) gen.gtypes
		end;

		RenameTypeParameters.run gen.gtypes_list;

		mkdir_from_path gen.gcon.file;

		List.iter (fun md_def ->
			let source_dir = gen.gcon.file ^ "/src/" ^ (String.concat "/" (fst (path_of_md_def md_def))) in
			let w = SourceWriter.new_source_writer() in
			let should_write = List.fold_left (fun should md -> module_type_gen w md || should) false md_def.m_types in
			if should_write then begin
				let path = path_of_md_def md_def in
				write_file gen w source_dir path "cs" out_files
			end
		) gen.gmodules;

		if not (Common.defined gen.gcon Define.KeepOldOutput) then
			clean_files (gen.gcon.file ^ "/src") !out_files gen.gcon.verbose;

		dump_descriptor gen ("hxcs_build.txt") s_type_path module_s;
		if ( not (Common.defined gen.gcon Define.NoCompilation) ) then begin
			let old_dir = Sys.getcwd() in
			Sys.chdir gen.gcon.file;
			let cmd = "haxelib run hxcs hxcs_build.txt --haxe-version " ^ (string_of_int gen.gcon.version) ^ " --feature-level 1" in
			print_endline cmd;
			if gen.gcon.run_command cmd <> 0 then failwith "Build failed";
			Sys.chdir old_dir;
		end

	with TypeNotFound path ->
		con.error ("Error. Module '" ^ (s_type_path path) ^ "' is required and was not included in build.") null_pos);
	debug_mode := false
