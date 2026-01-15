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

(* C# code generator - main coordinator *)

open Globals
open Ast
open Type
open Gctx
open CsGlobals
open CsAst
open CsSignature
open CsPrinter

(* Generation context *)
type gen_context = {
	com : Gctx.t;
	mutable generated_types : cs_type_def list;
	mutable main_class : path option;
}

let create_context com = {
	com = com;
	generated_types = [];
	main_class = None;
}

(* Convert Haxe binop to C# binop *)
let rec cs_binop_of_binop = function
	| OpAdd -> CsOpAdd
	| OpSub -> CsOpSub
	| OpMult -> CsOpMul
	| OpDiv -> CsOpDiv
	| OpMod -> CsOpMod
	| OpAnd -> CsOpBoolAnd
	| OpOr -> CsOpBoolOr
	| OpXor -> CsOpXor
	| OpShl -> CsOpShl
	| OpShr -> CsOpShr
	| OpUShr -> CsOpShr  (* C# doesn't have unsigned shift, handle separately *)
	| OpEq -> CsOpEq
	| OpNotEq -> CsOpNotEq
	| OpLt -> CsOpLt
	| OpLte -> CsOpLte
	| OpGt -> CsOpGt
	| OpGte -> CsOpGte
	| OpAssign -> CsOpAssign
	| OpBoolAnd -> CsOpBoolAnd
	| OpBoolOr -> CsOpBoolOr
	| OpInterval -> failwith "Interval operator not supported"
	| OpArrow -> failwith "Arrow operator not supported"
	| OpIn -> failwith "In operator not supported"
	| OpNullCoal -> CsOpNullCoalesce
	| OpAssignOp op -> CsOpAssignOp (cs_binop_of_binop op)

(* Convert Haxe unop to C# unop *)
let cs_unop_of_unop = function
	| Increment -> CsOpIncrement
	| Decrement -> CsOpDecrement
	| Not -> CsOpNot
	| Neg -> CsOpNeg
	| NegBits -> CsOpBitNot
	| Spread -> failwith "Spread operator not supported"

(* Expression generation context *)
type expr_context = {
	gctx : gen_context;
	mutable local_vars : (int * string) list;  (* tvar.v_id -> generated name *)
	mutable temp_count : int;
}

let create_expr_context gctx = {
	gctx = gctx;
	local_vars = [];
	temp_count = 0;
}

(* Get or generate name for local variable *)
let get_local_name ectx v =
	try
		List.assoc v.v_id ectx.local_vars
	with Not_found ->
		let name = escape_identifier v.v_name in
		ectx.local_vars <- (v.v_id, name) :: ectx.local_vars;
		name

(* Generate fresh temp variable name *)
let fresh_temp ectx =
	ectx.temp_count <- ectx.temp_count + 1;
	Printf.sprintf "_hx_tmp%d" ectx.temp_count

(* Convert Haxe constant to C# constant *)
let cs_const_of_tconst = function
	| TInt i -> CsConstInt i
	| TFloat s -> CsConstDouble (float_of_string s)
	| TString s -> CsConstString s
	| TBool b -> CsConstBool b
	| TNull -> CsConstNull
	| TThis -> failwith "TThis is not a constant"
	| TSuper -> failwith "TSuper is not a constant"

(* Convert Haxe expression to C# expression - mutually recursive with cs_stmt_of_texpr *)
let rec cs_expr_of_texpr ectx e =
	match e.eexpr with
	| TConst TThis -> CsThis
	| TConst TSuper -> CsBase
	| TConst TNull -> CsNull
	| TConst c -> CsConst (cs_const_of_tconst c)
	| TLocal v -> CsLocal (get_local_name ectx v)
	| TArray (e1, e2) ->
		CsArrayAccess (cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
	| TBinop (op, e1, e2) ->
		CsBinop (cs_binop_of_binop op, cs_expr_of_texpr ectx e1, cs_expr_of_texpr ectx e2)
	| TUnop (op, Prefix, e) ->
		CsUnop (cs_unop_of_unop op, false, cs_expr_of_texpr ectx e)
	| TUnop (op, Postfix, e) ->
		CsUnop (cs_unop_of_unop op, true, cs_expr_of_texpr ectx e)
	| TField (e, FInstance (_, _, cf))
	| TField (e, FClosure (_, cf)) ->
		CsField (cs_expr_of_texpr ectx e, escape_identifier cf.cf_name)
	| TField (_, FStatic (c, cf)) ->
		let path = cs_path_of_path c.cl_path in
		CsStaticField (CsTypeClass (path, []), escape_identifier cf.cf_name)
	| TField (e, FAnon cf) ->
		CsField (cs_expr_of_texpr ectx e, escape_identifier cf.cf_name)
	| TField (e, FDynamic name) ->
		CsField (cs_expr_of_texpr ectx e, escape_identifier name)
	| TField (_, FEnum (en, ef)) ->
		let path = cs_path_of_path en.e_path in
		CsStaticField (CsTypeClass (path, []), escape_identifier ef.ef_name)
	| TCall ({ eexpr = TField (e, FInstance (_, _, cf)) }, args)
	| TCall ({ eexpr = TField (e, FClosure (_, cf)) }, args) ->
		let obj = cs_expr_of_texpr ectx e in
		let args = List.map (cs_expr_of_texpr ectx) args in
		CsCall (CsField (obj, escape_identifier cf.cf_name), args)
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args) ->
		let path = cs_path_of_path c.cl_path in
		let args = List.map (cs_expr_of_texpr ectx) args in
		CsStaticCall (CsTypeClass (path, []), escape_identifier cf.cf_name, args)
	| TCall (e, args) ->
		let func = cs_expr_of_texpr ectx e in
		let args = List.map (cs_expr_of_texpr ectx) args in
		CsCall (func, args)
	| TNew (c, _, args) ->
		let path = cs_path_of_path c.cl_path in
		let args = List.map (cs_expr_of_texpr ectx) args in
		CsNew (CsTypeClass (path, []), args)
	| TObjectDecl fields ->
		(* For now, create anonymous object - will need HaxeDynamicObject *)
		let _ = List.map (fun ((name, _, _), e) ->
			(name, cs_expr_of_texpr ectx e)
		) fields in
		CsNew (CsTypeClass (NativeTypes.haxe_dynamic_object_path, []), [])
	| TArrayDecl items ->
		let items = List.map (cs_expr_of_texpr ectx) items in
		let elem_type = cs_type_of_type ectx.gctx e.etype in
		CsNewArray (elem_type, items)
	| TTypeExpr (TClassDecl c) ->
		CsTypeOf (CsTypeClass (cs_path_of_path c.cl_path, []))
	| TTypeExpr (TEnumDecl e) ->
		CsTypeOf (CsTypeClass (cs_path_of_path e.e_path, []))
	| TTypeExpr _ ->
		CsTypeOf CsTypeObject
	| TParenthesis e ->
		CsParens (cs_expr_of_texpr ectx e)
	| TCast (e, _) ->
		let target_type = cs_type_of_type ectx.gctx e.etype in
		CsCast (target_type, cs_expr_of_texpr ectx e)
	| TThrow e ->
		CsThrow (cs_expr_of_texpr ectx e)
	| TMeta (_, e) ->
		cs_expr_of_texpr ectx e
	| TBlock _
	| TIf _
	| TWhile _
	| TSwitch _
	| TTry _
	| TReturn _
	| TBreak
	| TContinue
	| TVar _ ->
		(* These need to be handled as statements, not expressions *)
		(* For block expressions, we'd need to generate a local function *)
		CsRaw "/* TODO: block expression */"
	| TFunction tf ->
		(* Lambda/closure *)
		let params = List.map (fun (v, _) ->
			{
				p_name = get_local_name ectx v;
				p_type = Some (cs_type_of_type ectx.gctx v.v_type);
				p_default = None;
				p_modifier = None;
			}
		) tf.tf_args in
		let body = match tf.tf_expr.eexpr with
			| TBlock exprs -> CsLambdaBlock (List.map (cs_stmt_of_texpr ectx) exprs)
			| TReturn (Some e) -> CsLambdaExpr (cs_expr_of_texpr ectx e)
			| _ -> CsLambdaBlock [cs_stmt_of_texpr ectx tf.tf_expr]
		in
		CsLambda (params, body)
	| TEnumParameter (e, ef, i) ->
		(* Access enum constructor parameter *)
		let obj = cs_expr_of_texpr ectx e in
		let param_name = match ef.ef_type with
			| TFun (args, _) when i < List.length args ->
				let (name, _, _) = List.nth args i in
				name
			| _ -> Printf.sprintf "_hx_p%d" i
		in
		CsField (obj, escape_identifier param_name)
	| TEnumIndex e ->
		CsField (cs_expr_of_texpr ectx e, "_hx_index")
	| TIdent s ->
		CsLocal (escape_identifier s)

(* Convert Haxe expression to C# statement - mutually recursive with cs_expr_of_texpr *)
and cs_stmt_of_texpr ectx e =
	match e.eexpr with
	| TBlock exprs ->
		CsBlock (List.map (cs_stmt_of_texpr ectx) exprs)
	| TVar (v, init) ->
		let name = get_local_name ectx v in
		let cs_type = Some (cs_type_of_type ectx.gctx v.v_type) in
		let init = Option.map (cs_expr_of_texpr ectx) init in
		CsVarDecl (name, cs_type, init)
	| TIf (cond, then_expr, else_expr) ->
		let cond = cs_expr_of_texpr ectx cond in
		let then_stmt = cs_stmt_of_texpr ectx then_expr in
		let else_stmt = Option.map (cs_stmt_of_texpr ectx) else_expr in
		CsIf (cond, then_stmt, else_stmt)
	| TWhile (cond, body, NormalWhile) ->
		let cond = cs_expr_of_texpr ectx cond in
		let body = cs_stmt_of_texpr ectx body in
		CsWhile (cond, body)
	| TWhile (cond, body, DoWhile) ->
		let cond = cs_expr_of_texpr ectx cond in
		let body = cs_stmt_of_texpr ectx body in
		CsDoWhile (body, cond)
	| TSwitch sw ->
		let cond = cs_expr_of_texpr ectx sw.switch_subject in
		let sections = List.map (fun case ->
			let labels = List.map (fun p ->
				CsCaseConst (cs_expr_of_texpr ectx p)
			) case.case_patterns in
			let body_stmts = [cs_stmt_of_texpr ectx case.case_expr; CsBreak] in
			{ sw_labels = labels; sw_body = body_stmts }
		) sw.switch_cases in
		let sections = match sw.switch_default with
			| None -> sections
			| Some e ->
				let default_section = {
					sw_labels = [CsDefault];
					sw_body = [cs_stmt_of_texpr ectx e; CsBreak]
				} in
				sections @ [default_section]
		in
		CsSwitch (cond, sections)
	| TTry (body, catches) ->
		let body = cs_stmt_of_texpr ectx body in
		let catches = List.map (fun (v, e) ->
			let name = get_local_name ectx v in
			let cs_type = cs_type_of_type ectx.gctx v.v_type in
			let body = cs_stmt_of_texpr ectx e in
			{
				catch_type = Some cs_type;
				catch_name = Some name;
				catch_when = None;
				catch_body = body;
			}
		) catches in
		CsTry (body, catches, None)
	| TReturn None ->
		CsReturn None
	| TReturn (Some e) ->
		CsReturn (Some (cs_expr_of_texpr ectx e))
	| TBreak ->
		CsBreak
	| TContinue ->
		CsContinue
	| TThrow e ->
		CsThrowStmt (cs_expr_of_texpr ectx e)
	| _ ->
		(* Expression statement *)
		CsExprStmt (cs_expr_of_texpr ectx e)

(* Generate method body *)
let generate_method_body gctx e =
	let ectx = create_expr_context gctx in
	match e.eexpr with
	| TFunction tf ->
		(* Unwrap TFunction - this happens for dynamic function assignments *)
		(* Register parameter names in context *)
		List.iter (fun (v, _) ->
			ignore (get_local_name ectx v)
		) tf.tf_args;
		(* Generate the inner body *)
		begin match tf.tf_expr.eexpr with
		| TBlock exprs -> List.map (cs_stmt_of_texpr ectx) exprs
		| _ -> [cs_stmt_of_texpr ectx tf.tf_expr]
		end
	| TBlock exprs ->
		List.map (cs_stmt_of_texpr ectx) exprs
	| _ ->
		[cs_stmt_of_texpr ectx e]

(* Generate class field as C# member *)
let generate_field gctx c cf is_static =
	let name = escape_identifier cf.cf_name in
	let cs_type = cs_type_of_type gctx cf.cf_type in
	let modifiers = if is_static then [MemberModifier.Static] else [] in

	match cf.cf_kind with
	| Var { v_read = AccNormal; v_write = AccNormal }
	| Var { v_read = AccNormal; v_write = AccNever } ->
		(* Simple field *)
		let value = match cf.cf_expr with
			| Some e ->
				let ectx = create_expr_context gctx in
				Some (cs_expr_of_texpr ectx e)
			| None -> None
		in
		Some (CsMemberField {
			f_name = name;
			f_type = cs_type;
			f_access = AccessModifier.Public;
			f_modifiers = modifiers;
			f_value = value;
		})
	| Var _ ->
		(* Property *)
		Some (CsMemberProperty {
			prop_name = name;
			prop_type = cs_type;
			prop_access = AccessModifier.Public;
			prop_modifiers = modifiers;
			prop_getter = Some { acc_access = None; acc_body = None };
			prop_setter = Some { acc_access = None; acc_body = None };
			prop_init = None;
		})
	| Method MethNormal | Method MethInline ->
		(* Regular method *)
		let args, ret = match follow cf.cf_type with
			| TFun (args, ret) -> args, ret
			| _ -> [], cf.cf_type
		in
		let params = List.map (fun (n, opt, t) ->
			{
				p_name = escape_identifier n;
				p_type = Some (cs_type_of_type gctx t);
				p_default = if opt then Some CsNull else None;
				p_modifier = None;
			}
		) args in
		let body = match cf.cf_expr with
			| Some e -> Some (generate_method_body gctx e)
			| None -> None
		in
		Some (CsMemberMethod {
			m_name = name;
			m_return_type = cs_type_of_type gctx ret;
			m_access = AccessModifier.Public;
			m_modifiers = modifiers;
			m_type_params = [];
			m_params = params;
			m_body = body;
			m_constraints = [];
		})
	| Method MethDynamic ->
		(* Dynamic method - generate as field with function type *)
		None
	| Method MethMacro ->
		(* Macro method - skip *)
		None

(* Generate constructor *)
let generate_constructor gctx c cf =
	let args = match follow cf.cf_type with
		| TFun (args, _) -> args
		| _ -> []
	in
	let params = List.map (fun (n, opt, t) ->
		{
			p_name = escape_identifier n;
			p_type = Some (cs_type_of_type gctx t);
			p_default = if opt then Some CsNull else None;
			p_modifier = None;
		}
	) args in
	let body = match cf.cf_expr with
		| Some e -> generate_method_body gctx e
		| None -> []
	in
	CsMemberConstructor {
		ctor_access = AccessModifier.Public;
		ctor_modifiers = [];
		ctor_params = params;
		ctor_base_call = None;
		ctor_this_call = None;
		ctor_body = body;
	}

(* Generate C# class from Haxe class *)
let generate_class gctx c =
	let path = cs_path_of_path c.cl_path in

	(* Check if this is the main class *)
	begin match gctx.com.main.main_path with
	| Some mp when mp = c.cl_path -> gctx.main_class <- Some c.cl_path
	| _ -> ()
	end;

	(* Determine modifiers *)
	let modifiers =
		(if (has_class_flag c CFinal) then [TypeModifier.Sealed] else []) @
		(if (has_class_flag c CAbstract) then [TypeModifier.Abstract] else [])
	in

	(* Generate base class reference *)
	let base_class = match c.cl_super with
		| Some (sc, params) ->
			let path = cs_path_of_path sc.cl_path in
			let params = List.map (cs_type_of_type gctx) params in
			Some (CsTypeClass (path, params))
		| None -> None
	in

	(* Generate interface references *)
	let interfaces = List.map (fun (i, params) ->
		let path = cs_path_of_path i.cl_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	) c.cl_implements in

	(* Generate members *)
	let members = ref [] in

	(* Constructor *)
	begin match c.cl_constructor with
	| Some cf -> members := generate_constructor gctx c cf :: !members
	| None -> ()
	end;

	(* Fields *)
	List.iter (fun cf ->
		match generate_field gctx c cf false with
		| Some m -> members := m :: !members
		| None -> ()
	) c.cl_ordered_fields;

	(* Static fields *)
	List.iter (fun cf ->
		match generate_field gctx c cf true with
		| Some m -> members := m :: !members
		| None -> ()
	) c.cl_ordered_statics;

	CsClassDef {
		c_path = path;
		c_access = AccessModifier.Public;
		c_modifiers = modifiers;
		c_type_params = [];  (* TODO: handle type parameters *)
		c_base = base_class;
		c_interfaces = interfaces;
		c_constraints = [];
		c_members = List.rev !members;
	}

(* Generate C# interface from Haxe interface *)
let generate_interface gctx c =
	let path = cs_path_of_path c.cl_path in

	(* Generate base interfaces *)
	let base_interfaces = List.map (fun (i, params) ->
		let path = cs_path_of_path i.cl_path in
		let params = List.map (cs_type_of_type gctx) params in
		CsTypeClass (path, params)
	) c.cl_implements in

	(* Generate members *)
	let members = List.filter_map (fun cf ->
		match cf.cf_kind with
		| Method MethNormal ->
			let args, ret = match follow cf.cf_type with
				| TFun (args, ret) -> args, ret
				| _ -> [], cf.cf_type
			in
			let params = List.map (fun (n, _, t) ->
				{
					p_name = escape_identifier n;
					p_type = Some (cs_type_of_type gctx t);
					p_default = None;
					p_modifier = None;
				}
			) args in
			Some (CsMemberMethod {
				m_name = escape_identifier cf.cf_name;
				m_return_type = cs_type_of_type gctx ret;
				m_access = AccessModifier.Public;
				m_modifiers = [];
				m_type_params = [];
				m_params = params;
				m_body = None;  (* Interface methods have no body *)
				m_constraints = [];
			})
		| _ -> None
	) c.cl_ordered_fields in

	CsInterfaceDef {
		i_path = path;
		i_access = AccessModifier.Public;
		i_modifiers = [];
		i_type_params = [];
		i_base = base_interfaces;
		i_constraints = [];
		i_members = members;
	}

(* Generate C# enum from Haxe enum *)
let generate_enum gctx (e : tenum) =
	let path = cs_path_of_path e.e_path in

	(* Check if this is a simple enum (no constructors with parameters) *)
	let is_simple = PMap.fold (fun ef acc ->
		acc && (match ef.ef_type with TFun _ -> false | _ -> true)
	) e.e_constrs true in

	if is_simple then
		(* Generate as C# enum *)
		let members = PMap.fold (fun ef acc ->
			{ em_name = escape_identifier ef.ef_name; em_value = None } :: acc
		) e.e_constrs [] in
		CsEnumDef {
			e_path = path;
			e_access = AccessModifier.Public;
			e_underlying = None;
			e_members = List.rev members;
		}
	else
		(* Generate as abstract class with nested classes *)
		let members = PMap.fold (fun ef acc ->
			match ef.ef_type with
			| TFun (args, _) ->
				(* Nested class for constructor with parameters *)
				let class_name = escape_identifier ef.ef_name in
				let fields = List.mapi (fun i (name, _, t) ->
					CsMemberField {
						f_name = escape_identifier name;
						f_type = cs_type_of_type gctx t;
						f_access = AccessModifier.Public;
						f_modifiers = [];
						f_value = None;
					}
				) args in
				let ctor_params = List.map (fun (name, _, t) ->
					{
						p_name = escape_identifier name;
						p_type = Some (cs_type_of_type gctx t);
						p_default = None;
						p_modifier = None;
					}
				) args in
				let ctor_body = List.map (fun (name, _, _) ->
					let esc_name = escape_identifier name in
					CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, esc_name),
						CsLocal esc_name))
				) args @ [
					CsExprStmt (CsBinop (CsOpAssign,
						CsField (CsThis, "_hx_index"),
						CsConst (CsConstInt (Int32.of_int ef.ef_index))))
				] in
				let ctor = CsMemberConstructor {
					ctor_access = AccessModifier.Public;
					ctor_modifiers = [];
					ctor_params = ctor_params;
					ctor_base_call = None;
					ctor_this_call = None;
					ctor_body = ctor_body;
				} in
				let nested_class = CsClassDef {
					c_path = (fst path, class_name);
					c_access = AccessModifier.Public;
					c_modifiers = [];
					c_type_params = [];
					c_base = Some (CsTypeClass (path, []));
					c_interfaces = [];
					c_constraints = [];
					c_members = fields @ [ctor];
				} in
				CsMemberNestedType nested_class :: acc
			| _ ->
				(* Static field for simple constructor *)
				let field = CsMemberField {
					f_name = escape_identifier ef.ef_name;
					f_type = CsTypeClass (path, []);
					f_access = AccessModifier.Public;
					f_modifiers = [MemberModifier.Static; MemberModifier.Readonly];
					f_value = Some (CsNew (CsTypeClass (path, []), []));
				} in
				field :: acc
		) e.e_constrs [] in

		(* Add _hx_index field *)
		let index_field = CsMemberField {
			f_name = "_hx_index";
			f_type = CsTypeInt;
			f_access = AccessModifier.Public;
			f_modifiers = [];
			f_value = None;
		} in

		CsClassDef {
			c_path = path;
			c_access = AccessModifier.Public;
			c_modifiers = [TypeModifier.Abstract];
			c_type_params = [];
			c_base = None;
			c_interfaces = [];
			c_constraints = [];
			c_members = index_field :: List.rev members;
		}

(* Generate type *)
let generate_type gctx mt =
	match mt with
	| TClassDecl c when not (has_class_flag c CInterface) ->
		Some (generate_class gctx c)
	| TClassDecl c ->
		Some (generate_interface gctx c)
	| TEnumDecl e ->
		Some (generate_enum gctx e)
	| TTypeDecl _ | TAbstractDecl _ ->
		None

(* Write file to disk *)
let write_file base_path rel_path content =
	let full_path = base_path ^ "/" ^ rel_path in
	Path.mkdir_from_path full_path;
	let ch = open_out_bin full_path in
	output_string ch content;
	close_out ch

(* Main generation entry point *)
let generate com =
	let gctx = create_context com in

	(* Generate all types *)
	List.iter (fun mt ->
		match generate_type gctx mt with
		| Some td -> gctx.generated_types <- td :: gctx.generated_types
		| None -> ()
	) com.types;

	(* Group types by namespace and write files *)
	let files = List.map (fun td ->
		let path = match td with
			| CsClassDef c -> c.c_path
			| CsStructDef s -> s.s_path
			| CsInterfaceDef i -> i.i_path
			| CsEnumDef e -> e.e_path
			| CsDelegateDef d -> d.d_path
		in
		let namespace = fst path in
		let name = snd path in
		let file = {
			file_usings = [
				CsUsingNamespace ["System"];
				CsUsingNamespace ["System"; "Collections"; "Generic"];
			];
			file_namespace = if namespace = [] then None else Some namespace;
			file_types = [td];
		} in
		let rel_path = String.concat "/" namespace ^ "/" ^ name ^ ".cs" in
		(rel_path, generate_file file)
	) (List.rev gctx.generated_types) in

	(* Write all files *)
	List.iter (fun (rel_path, content) ->
		write_file com.file rel_path content
	) files;

	(* Generate Program.cs with Main entry point if we have a main class *)
	begin match gctx.main_class with
	| Some main_path ->
		let main_class_path = cs_path_of_path main_path in
		let program_content = Printf.sprintf
"// Generated by Haxe C# target
using System;

public class Program
{
    public static void Main(string[] args)
    {
        %s.main();
    }
}
" (s_cs_path main_class_path) in
		write_file com.file "Program.cs" program_content
	| None -> ()
	end;

	(* Generate .csproj *)
	let proj = {
		proj_name = "HaxeProject";
		proj_target_framework = "net8.0";
		proj_output_type = "Exe";
	} in
	write_file com.file "Project.csproj" (generate_csproj proj)
