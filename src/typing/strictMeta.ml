open Globals
open Ast
open Type
open Common
open Typecore

let get_native_repr md pos =
	let path, meta = match md with
		| TClassDecl cl -> cl.cl_path, cl.cl_meta
		| TEnumDecl e -> e.e_path, e.e_meta
		| TTypeDecl t -> t.t_path, t.t_meta
		| TAbstractDecl a -> a.a_path, a.a_meta
	in
	let rec loop acc = function
		| (Meta.Native,[EConst(String(name,_)),_],_) :: meta ->
			loop (Ast.parse_path name) meta
		| _ :: meta ->
			loop acc meta
		| [] ->
			acc
	in
	let pack, name = loop path meta in
	match pack with
		| [] ->
			(EConst(Ident(name)), pos)
		| hd :: tl ->
			let rec loop pack expr = match pack with
				| hd :: tl ->
					loop tl (efield(expr,hd),pos)
				| [] ->
					(efield(expr,name),pos)
			in
			loop tl (EConst(Ident(hd)),pos)

let rec process_meta_argument ?(toplevel=true) ctx expr = match expr.eexpr with
	| TField(e,f) ->
		(efield(process_meta_argument ~toplevel:false ctx e,field_name f),expr.epos)
	| TConst(TInt i) ->
		(EConst(Int (Int32.to_string i, None)), expr.epos)
	| TConst(TFloat f) ->
		(EConst(Float (f, None)), expr.epos)
	| TConst(TString s) ->
		(EConst(String(s,SDoubleQuotes)), expr.epos)
	| TConst TNull ->
		(EConst(Ident "null"), expr.epos)
	| TConst(TBool b) ->
		(EConst(Ident (string_of_bool b)), expr.epos)
	| TCast(e,_) | TMeta(_,e) | TParenthesis(e) ->
		process_meta_argument ~toplevel ctx e
	| TTypeExpr md when toplevel ->
		let p = expr.epos in
		(efield(get_native_repr md expr.epos, "class"), p)
	| TTypeExpr md ->
		get_native_repr md expr.epos
	| _ ->
		display_error ctx.com "This expression is too complex to be a strict metadata argument" expr.epos;
		(EConst(Ident "null"), expr.epos)

let handle_fields ctx fields_to_check with_type_expr =
	List.map (fun ((name,_,_),expr) ->
		let pos = snd expr in
		let field = (efield(with_type_expr,name), pos) in
		let fieldexpr = (EConst(Ident name),pos) in
		let left_side = match ctx.com.platform with
			| Jvm -> (ECall(field,[]),pos)
			| _ -> die "" __LOC__
		in

		let left = type_expr ctx left_side NoValue in
		let right = type_expr ctx expr (WithType.with_type left.etype) in
		unify ctx left.etype right.etype (snd expr);
		(EBinop(Ast.OpAssign,fieldexpr,process_meta_argument ctx right), pos)
	) fields_to_check

let make_meta ctx texpr extra =
	match texpr.eexpr with
		| TNew(c,_,el) ->
			ECall(get_native_repr (TClassDecl c) texpr.epos, (List.map (process_meta_argument ctx) el) @ extra), texpr.epos
		| TTypeExpr(md) ->
			ECall(get_native_repr md texpr.epos, extra), texpr.epos
		| _ ->
			display_error ctx.com "Unexpected expression" texpr.epos; die "" __LOC__

let get_strict_meta ctx meta params pos =
	let pf = ctx.com.platform in
	let changed_expr, fields_to_check, ctype = match params with
		| [ECall(ef, el),p] ->
			let tpath = field_to_type_path ctx.com ef in
			begin match pf with
			| Jvm ->
				let fields = match el with
				| [EObjectDecl(fields),_] ->
					fields
				| [] ->
					[]
				| (_,p) :: _ ->
					display_error ctx.com "Object declaration expected" p;
					[]
				in
				ef, fields, CTPath tpath
			| _ ->
				Error.typing_error "@:strict is not supported on this target" p
			end
		| [EConst(Ident i),p as expr] ->
			let tpath = { tpackage=[]; tname=i; tparams=[]; tsub=None } in
			expr, [], CTPath tpath
		| [ (EField(_),p as field) ] ->
			let tpath = field_to_type_path ctx.com field in
			field, [], CTPath tpath
		| _ ->
			display_error ctx.com "A @:strict metadata must contain exactly one parameter. Please check the documentation for more information" pos;
			raise Exit
	in
	let texpr = type_expr ctx changed_expr NoValue in
	let with_type_expr = (ECheckType( (EConst (Ident "null"), pos), (ctype,null_pos) ), pos) in
	let extra = handle_fields ctx fields_to_check with_type_expr in
	meta, [make_meta ctx texpr extra], pos

let check_strict_meta ctx metas =
	let pf = ctx.com.platform in
	match pf with
		| Jvm ->
			let ret = ref [] in
			List.iter (function
				| Meta.Strict,params,pos -> (try
					ret := get_strict_meta ctx Meta.Meta params pos :: !ret
				with | Exit -> ())
				| _ -> ()
			) metas;
			!ret
		| _ -> []