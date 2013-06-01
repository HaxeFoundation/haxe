(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

open Ast

type error_msg =
	| Unexpected of token
	| Duplicate_default
	| Missing_semicolon
	| Unclosed_macro
	| Unimplemented
	| Missing_type
	| Custom of string

exception Error of error_msg * pos
exception TypePath of string list * (string * bool) option
exception Display of expr

let error_msg = function
	| Unexpected t -> "Unexpected "^(s_token t)
	| Duplicate_default -> "Duplicate default"
	| Missing_semicolon -> "Missing ;"
	| Unclosed_macro -> "Unclosed macro"
	| Unimplemented -> "Not implemented for current platform"
	| Missing_type -> "Missing type declaration"
	| Custom s -> s

let error m p = raise (Error (m,p))
let display_error : (error_msg -> pos -> unit) ref = ref (fun _ _ -> assert false)

let quoted_ident_prefix = "@$__hx__"

let quote_ident s =
	try
		for i = 0 to String.length s - 1 do
			match String.unsafe_get s i with
			| 'a'..'z' | 'A'..'Z' | '_' -> ()
			| '0'..'9' when i > 0 -> ()
			| _ -> raise Exit
		done;
		if Hashtbl.mem Lexer.keywords s then raise Exit;
		s
	with Exit ->
		quoted_ident_prefix ^ s

let cache = ref (DynArray.create())
let last_doc = ref None
let use_doc = ref false
let resume_display = ref null_pos
let in_macro = ref false

let last_token s =
	let n = Stream.count s in
	DynArray.get (!cache) (if n = 0 then 0 else n - 1)

let serror() = raise (Stream.Error "")

let do_resume() = !resume_display <> null_pos

let display e = raise (Display e)

let is_resuming p =
	let p2 = !resume_display in
	p.pmax = p2.pmin && Common.unique_full_path p.pfile = p2.pfile

let is_dollar_ident e = match fst e with
	| EConst (Ident n) when n.[0] = '$' ->
		true
	| _ ->
		false

let precedence op =
	let left = true and right = false in
	match op with
	| OpMod -> 0, left
	| OpMult | OpDiv -> 1, left
	| OpAdd | OpSub -> 2, left
	| OpShl | OpShr | OpUShr -> 3, left
	| OpOr | OpAnd | OpXor -> 4, left
	| OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte -> 5, left
	| OpInterval -> 6, left
	| OpBoolAnd -> 7, left
	| OpBoolOr -> 8, left
	| OpArrow -> 9, right
	| OpAssign | OpAssignOp _ -> 10, right

let is_not_assign = function
	| OpAssign | OpAssignOp _ -> false
	| _ -> true

let swap op1 op2 =
	let p1, left1 = precedence op1 in
	let p2, _ = precedence op2 in
	left1 && p1 <= p2

let rec make_binop op e ((v,p2) as e2) =
	match v with
	| EBinop (_op,_e,_e2) when swap op _op ->
		let _e = make_binop op e _e in
		EBinop (_op,_e,_e2) , punion (pos _e) (pos _e2)
	| ETernary (e1,e2,e3) when is_not_assign op ->
		let e = make_binop op e e1 in
		ETernary (e,e2,e3) , punion (pos e) (pos e3)
	| _ ->
		EBinop (op,e,e2) , punion (pos e) (pos e2)

let rec make_unop op ((v,p2) as e) p1 =
	match v with
	| EBinop (bop,e,e2) -> EBinop (bop, make_unop op e p1 , e2) , (punion p1 p2)
	| ETernary (e1,e2,e3) -> ETernary (make_unop op e1 p1 , e2, e3), punion p1 p2
	| _ ->
		EUnop (op,Prefix,e), punion p1 p2

let rec make_meta name params ((v,p2) as e) p1 =
	match v with
	| EBinop (bop,e,e2) -> EBinop (bop, make_meta name params e p1 , e2) , (punion p1 p2)
	| ETernary (e1,e2,e3) -> ETernary (make_meta name params e1 p1 , e2, e3), punion p1 p2
	| _ ->
		EMeta((name,params,p1),e),punion p1 p2

let reify in_macro =
	let cur_pos = ref None in
	let mk_enum ename n vl p =
		let constr = (EConst (Ident n),p) in
		match vl with
		| [] -> constr
		| _ -> (ECall (constr,vl),p)
	in
	let to_const c p =
		let cst n v = mk_enum "Constant" n [EConst (String v),p] p in
		match c with
		| Int i -> cst "CInt" i
		| String s -> cst "CString" s
		| Float s -> cst "CFloat" s
		| Ident s -> cst "CIdent" s
		| Regexp (r,o) -> mk_enum "Constant" "CRegexp" [(EConst (String r),p);(EConst (String o),p)] p
	in
	let rec to_binop o p =
		let op n = mk_enum "Binop" n [] p in
		match o with
		| OpAdd -> op "OpAdd"
		| OpMult -> op "OpMult"
		| OpDiv -> op "OpDiv"
		| OpSub -> op "OpSub"
		| OpAssign -> op "OpAssign"
		| OpEq -> op "OpEq"
		| OpNotEq -> op "OpNotEq"
		| OpGt -> op "OpGt"
		| OpGte -> op "OpGte"
		| OpLt -> op "OpLt"
		| OpLte -> op "OpLte"
		| OpAnd -> op "OpAnd"
		| OpOr -> op "OpOr"
		| OpXor -> op "OpXor"
		| OpBoolAnd -> op "OpBoolAnd"
		| OpBoolOr -> op "OpBoolOr"
		| OpShl -> op "OpShl"
		| OpShr -> op "OpShr"
		| OpUShr -> op "OpUShr"
		| OpMod -> op "OpMod"
		| OpAssignOp o -> mk_enum "Binop" "OpAssignOp" [to_binop o p] p
		| OpInterval -> op "OpInterval"
		| OpArrow -> op "OpArrow"
	in
	let to_string s p =
		let len = String.length s in
		if len > 1 && s.[0] = '$' then
			(EConst (Ident (String.sub s 1 (len - 1))),p)
		else
			(EConst (String s),p)
	in
	let to_array f a p =
		(EArrayDecl (List.map (fun s -> f s p) a),p)
	in
	let to_null p =
		(EConst (Ident "null"),p)
	in
	let to_opt f v p =
		match v with
		| None -> to_null p
		| Some v -> f v p
	in
	let to_bool o p =
		(EConst (Ident (if o then "true" else "false")),p)
	in
	let to_obj fields p =
		(EObjectDecl fields,p)
	in
	let rec to_tparam t p =
		let n, v = (match t with
			| TPType t -> "TPType", to_ctype t p
			| TPExpr e -> "TPExpr", to_expr e p
		) in
		mk_enum "TypeParam" n [v] p
	and to_tpath t p =
		let fields = [
			("pack", to_array to_string t.tpackage p);
			("name", to_string t.tname p);
			("params", to_array to_tparam t.tparams p);
		] in
		to_obj (match t.tsub with None -> fields | Some s -> fields @ ["sub",to_string s p]) p
	and to_ctype t p =
		let ct n vl = mk_enum "ComplexType" n vl p in
		match t with
		| CTPath { tpackage = []; tparams = []; tsub = None; tname = n } when n.[0] = '$' ->
			to_string n p
		| CTPath t -> ct "TPath" [to_tpath t p]
		| CTFunction (args,ret) -> ct "TFunction" [to_array to_ctype args p; to_ctype ret p]
		| CTAnonymous fields -> ct "TAnonymous" [to_array to_cfield fields p]
		| CTParent t -> ct "TParent" [to_ctype t p]
		| CTExtend (t,fields) -> ct "TExtend" [to_tpath t p; to_array to_cfield fields p]
		| CTOptional t -> ct "TOptional" [to_ctype t p]
	and to_fun f p =
		let farg (n,o,t,e) p =
			let fields = [
				"name", to_string n p;
				"opt", to_bool o p;
				"type", to_opt to_ctype t p;
			] in
			to_obj (match e with None -> fields | Some e -> fields @ ["value",to_expr e p]) p
		in
		let rec fparam t p =
			let fields = [
				"name", to_string t.tp_name p;
				"constraints", to_array to_ctype t.tp_constraints p;
				"params", to_array fparam t.tp_params p;
			] in
			to_obj fields p
		in
		let fields = [
			("args",to_array farg f.f_args p);
			("ret",to_opt to_ctype f.f_type p);
			("expr",to_opt to_expr f.f_expr p);
			("params",to_array fparam f.f_params p);
		] in
		to_obj fields p
	and to_cfield f p =
		let p = f.cff_pos in
		let to_access a p =
			let n = (match a with
			| APublic -> "APublic"
			| APrivate -> "APrivate"
			| AStatic -> "AStatic"
			| AOverride -> "AOverride"
			| ADynamic -> "ADynamic"
			| AInline -> "AInline"
			| AMacro -> "AMacro"
			) in
			mk_enum "Access" n [] p
		in
		let to_kind k =
			let n, vl = (match k with
				| FVar (ct,e) -> "FVar", [to_opt to_ctype ct p;to_opt to_expr e p]
				| FFun f -> "FFun", [to_fun f p]
				| FProp (get,set,t,e) -> "FProp", [to_string get p; to_string set p; to_opt to_ctype t p; to_opt to_expr e p]
			) in
			mk_enum "FieldType" n vl p
		in
		let fields = [
			Some ("name", to_string f.cff_name p);
			(match f.cff_doc with None -> None | Some s -> Some ("doc", to_string s p));
			(match f.cff_access with [] -> None | l -> Some ("access", to_array to_access l p));
			Some ("kind", to_kind f.cff_kind);
			Some ("pos", to_pos f.cff_pos);
			(match f.cff_meta with [] -> None | l -> Some ("meta", to_meta f.cff_meta p));
		] in
		let fields = List.rev (List.fold_left (fun acc v -> match v with None -> acc | Some e -> e :: acc) [] fields) in
		to_obj fields p
	and to_meta m p =
		to_array (fun (m,el,p) _ ->
			let fields = [
				"name", to_string (fst (Common.MetaInfo.to_string m)) p;
				"params", to_expr_array el p;
				"pos", to_pos p;
			] in
			to_obj fields p
		) m p
	and to_pos p =
		match !cur_pos with
		| Some p ->
			p
		| None ->
		let file = (EConst (String p.pfile),p) in
		let pmin = (EConst (Int (string_of_int p.pmin)),p) in
		let pmax = (EConst (Int (string_of_int p.pmax)),p) in
		if in_macro then
			(EUntyped (ECall ((EConst (Ident "$mk_pos"),p),[file;pmin;pmax]),p),p)
		else
			to_obj [("file",file);("min",pmin);("max",pmax)] p
	and to_expr_array a p = match a with
		| [EMeta ((Meta.Dollar "a",[],_),e1),_] -> (match fst e1 with EArrayDecl el -> to_expr_array el p | _ -> e1)
		| _ -> to_array to_expr a p
	and to_expr e _ =
		let p = snd e in
		let expr n vl =
			let e = mk_enum "ExprDef" n vl p in
			to_obj [("expr",e);("pos",to_pos p)] p
		in
		let loop e = to_expr e (snd e) in
		match fst e with
		| EConst (Ident n) when n.[0] = '$' && String.length n > 1 ->
			to_string n p
		| EConst c ->
			expr "EConst" [to_const c p]
		| EArray (e1,e2) ->
			expr "EArray" [loop e1;loop e2]
		| EBinop (op,e1,e2) ->
			expr "EBinop" [to_binop op p; loop e1; loop e2]
		| EField (e,s) ->
			expr "EField" [loop e; to_string s p]
		| EParenthesis e ->
			expr "EParenthesis" [loop e]
		| EObjectDecl fl ->
			expr "EObjectDecl" [to_array (fun (f,e) -> to_obj [("field",to_string f p);("expr",loop e)]) fl p]
		| EArrayDecl el ->
			expr "EArrayDecl" [to_expr_array el p]
		| ECall (e,el) ->
			expr "ECall" [loop e;to_expr_array el p]
		| ENew (t,el) ->
			expr "ENew" [to_tpath t p;to_expr_array el p]
		| EUnop (op,flag,e) ->
			let op = mk_enum "Unop" (match op with
				| Increment -> "OpIncrement"
				| Decrement -> "OpDecrement"
				| Not -> "OpNot"
				| Neg -> "OpNeg"
				| NegBits -> "OpNegBits"
			) [] p in
			expr "EUnop" [op;to_bool (flag = Postfix) p;loop e]
		| EVars vl ->
			expr "EVars" [to_array (fun (v,t,e) p ->
				let fields = [
					"name", to_string v p;
					"type", to_opt to_ctype t p;
					"expr", to_opt to_expr e p;
				] in
				to_obj fields p
			) vl p]
		| EFunction (name,f) ->
			expr "EFunction" [to_opt to_string name p; to_fun f p]
		| EBlock el ->
			expr "EBlock" [to_expr_array el p]
		| EFor (e1,e2) ->
			expr "EFor" [loop e1;loop e2]
		| EIn (e1,e2) ->
			expr "EIn" [loop e1;loop e2]
		| EIf (e1,e2,eelse) ->
			expr "EIf" [loop e1;loop e2;to_opt to_expr eelse p]
		| EWhile (e1,e2,flag) ->
			expr "EWhile" [loop e1;loop e2;to_bool (flag = NormalWhile) p]
		| ESwitch (e1,cases,def) ->
			let scase (el,eg,e) p =
				to_obj [("values",to_expr_array el p);"guard",to_opt to_expr eg p;"expr",to_opt to_expr e p] p
			in
			expr "ESwitch" [loop e1;to_array scase cases p;to_opt (to_opt to_expr) def p]
		| ETry (e1,catches) ->
			let scatch (n,t,e) p =
				to_obj [("name",to_string n p);("type",to_ctype t p);("expr",loop e)] p
			in
			expr "ETry" [loop e1;to_array scatch catches p]
		| EReturn eo ->
			expr "EReturn" [to_opt to_expr eo p]
		| EBreak ->
			expr "EBreak" []
		| EContinue ->
			expr "EContinue" []
		| EUntyped e ->
			expr "EUntyped" [loop e]
		| EThrow e ->
			expr "EThrow" [loop e]
		| ECast (e,ct) ->
			expr "ECast" [loop e; to_opt to_ctype ct p]
		| EDisplay (e,flag) ->
			expr "EDisplay" [loop e; to_bool flag p]
		| EDisplayNew t ->
			expr "EDisplayNew" [to_tpath t p]
		| ETernary (e1,e2,e3) ->
			expr "ETernary" [loop e1;loop e2;loop e3]
		| ECheckType (e1,ct) ->
			expr "ECheckType" [loop e1; to_ctype ct p]
		| EMeta ((m,ml,p),e1) ->
			match m, ml with
			| Meta.Dollar ("" | "e"), _ ->
				e1
			| Meta.Dollar "a", _ ->
				expr "EArrayDecl" (match fst e1 with EArrayDecl el -> [to_expr_array el p] | _ -> [e1])
			| Meta.Dollar "b", _ ->
				expr "EBlock" [e1]
			(* TODO: can $v and $i be implemented better? *)
			| Meta.Dollar "v", _ ->
				(ECall ((EField ((EField ((EField ((EConst (Ident "haxe"),p),"macro"),p),"Context"),p),"makeExpr"),p),[e; to_pos (pos e)]),p)
			| Meta.Dollar "i", _ ->
				expr "EConst" [mk_enum "Constant" "CIdent" [e1] (pos e1)]
			| Meta.Dollar "p", _ ->
				(ECall ((EField ((EField ((EField ((EConst (Ident "haxe"),p),"macro"),p),"ExprTools"),p),"toFieldExpr"),p),[e]),p)
			| Meta.Custom ":pos", [pexpr] ->
				let old = !cur_pos in
				cur_pos := Some pexpr;
				let e = loop e1 in
				cur_pos := old;
				e
			| _ ->
				expr "EMeta" [to_obj [("name",to_string (fst (Common.MetaInfo.to_string m)) p);("params",to_expr_array ml p);("pos",to_pos p)] p;loop e1]
	and to_tparam_decl p t =
		to_obj [
			"name", to_string t.tp_name p;
			"params", (EArrayDecl (List.map (to_tparam_decl p) t.tp_params),p);
			"constraints", (EArrayDecl (List.map (fun t -> to_ctype t p) t.tp_constraints),p)
		] p
	and to_type_def (t,p) =
		match t with
		| EClass d ->
			let ext = ref None and impl = ref [] and interf = ref false in
			List.iter (function
				| HExtern | HPrivate -> ()
				| HInterface -> interf := true;
				| HExtends t -> ext := Some (to_tpath t p)
				| HImplements i -> impl := (to_tpath i p) :: !impl
			) d.d_flags;
			to_obj [
				"pack", (EArrayDecl [],p);
				"name", to_string d.d_name p;
				"pos", to_pos p;
				"meta", to_meta d.d_meta p;
				"params", (EArrayDecl (List.map (to_tparam_decl p) d.d_params),p);
				"isExtern", to_bool (List.mem HExtern d.d_flags) p;
				"kind", mk_enum "TypeDefKind" "TDClass" [(match !ext with None -> (EConst (Ident "null"),p) | Some t -> t);(EArrayDecl (List.rev !impl),p);to_bool !interf p] p;
				"fields", (EArrayDecl (List.map (fun f -> to_cfield f p) d.d_data),p)
			] p
		| _ -> assert false
	in
	(fun e -> to_expr e (snd e)), to_ctype, to_type_def

let popt f = parser
	| [< v = f >] -> Some v
	| [< >] -> None

let rec plist f = parser
	| [< v = f; l = plist f >] -> v :: l
	| [< >] -> []

let rec psep sep f = parser
	| [< v = f; s >] ->
		let rec loop = parser
			| [< '(sep2,_) when sep2 = sep; v = f; l = loop >] -> v :: l
			| [< >] -> []
		in
		v :: loop s
	| [< >] -> []

let ident = parser
	| [< '(Const (Ident i),p) >] -> i,p

let dollar_ident = parser
	| [< '(Const (Ident i),p) >] -> i,p
	| [< '(Dollar i,p) >] -> ("$" ^ i),p

let dollar_ident_macro pack = parser
	| [< '(Const (Ident i),p) >] -> i,p
	| [< '(Dollar i,p) >] -> ("$" ^ i),p
	| [< '(Kwd Macro,p) when pack <> [] >] -> "macro", p

let lower_ident_or_macro = parser
	| [< '(Const (Ident i),p) when is_lower_ident i >] -> i
	| [< '(Kwd Macro,_) >] -> "macro"

let any_enum_ident = parser
	| [< i = ident >] -> i
	| [< '(Kwd k,p) when Filename.basename p.pfile = "StdTypes.hx" >] -> s_keyword k, p

let property_ident = parser
	| [< i, _ = ident >] -> i
	| [< '(Kwd Dynamic,_) >] -> "dynamic"
	| [< '(Kwd Default,_) >] -> "default"
	| [< '(Kwd Null,_) >] -> "null"

let get_doc s =
	(* do the peek first to make sure we fetch the doc *)
	match Stream.peek s with
	| None -> None
	| Some (tk,p) ->
		match !last_doc with
		| None -> None
		| Some (d,pos) ->
			last_doc := None;
			if pos = p.pmin then Some d else None

let comma = parser
	| [< '(Comma,_) >] -> ()

let semicolon s =
	if fst (last_token s) = BrClose then
		match s with parser
		| [< '(Semicolon,p) >] -> p
		| [< >] -> snd (last_token s)
	else
		match s with parser
		| [< '(Semicolon,p) >] -> p
		| [< s >] ->
			let pos = snd (last_token s) in
			if do_resume() then pos else error Missing_semicolon pos

let rec	parse_file s =
	last_doc := None;
	match s with parser
	| [< '(Kwd Package,_); p = parse_package; _ = semicolon; l = parse_type_decls p []; '(Eof,_) >] -> p , l
	| [< l = parse_type_decls [] []; '(Eof,_) >] -> [] , l

and parse_type_decls pack acc s =
	try
		match s with parser
		| [< v = parse_type_decl; l = parse_type_decls pack (v :: acc) >] -> l
		| [< >] -> List.rev acc
	with TypePath ([],Some (name,false)) ->
		(* resolve imports *)
		List.iter (fun d ->
			match fst d with
			| EImport (t,_) ->
				(match List.rev t with
				| (n,_) :: path when n = name && List.for_all (fun (i,_) -> is_lower_ident i) path -> raise (TypePath (List.map fst (List.rev path),Some (name,false)))
				| _ -> ())
			| _ -> ()
		) acc;
		raise (TypePath (pack,Some(name,true)))

and parse_type_decl s =
	match s with parser
	| [< '(Kwd Import,p1) >] -> parse_import s p1
	| [< '(Kwd Using,p1); t = parse_type_path; p2 = semicolon >] -> EUsing t, punion p1 p2
	| [< doc = get_doc; meta = parse_meta; c = parse_common_flags; s >] ->
		match s with parser
		| [< n , p1 = parse_enum_flags; name = type_name; tl = parse_constraint_params; '(BrOpen,_); l = plist parse_enum; '(BrClose,p2) >] ->
			(EEnum {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = List.map snd c @ n;
				d_data = l
			}, punion p1 p2)
		| [< n , p1 = parse_class_flags; name = type_name; tl = parse_constraint_params; hl = plist parse_class_herit; '(BrOpen,_); fl, p2 = parse_class_fields false p1 >] ->
			(EClass {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = List.map fst c @ n @ hl;
				d_data = fl;
			}, punion p1 p2)
		| [< '(Kwd Typedef,p1); name = type_name; tl = parse_constraint_params; '(Binop OpAssign,p2); t = parse_complex_type; s >] ->
			(match s with parser
			| [< '(Semicolon,_) >] -> ()
			| [< >] -> ());
			(ETypedef {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = List.map snd c;
				d_data = t;
			}, punion p1 p2)
		| [< '(Kwd Abstract,p1); name = type_name; tl = parse_constraint_params; st = parse_abstract_subtype; sl = plist parse_abstract_relations; '(BrOpen,_); fl, p2 = parse_class_fields false p1 >] ->
			let flags = List.map (fun (_,c) -> match c with EPrivate -> APrivAbstract | EExtern -> error (Custom "extern abstract not allowed") p1) c in
			let flags = (match st with None -> flags | Some t -> AIsType t :: flags) in
			(EAbstract {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = flags @ sl;
				d_data = fl;
			},punion p1 p2)

and parse_class doc meta cflags need_name s =
	let opt_name = if need_name then type_name else (fun s -> match popt type_name s with None -> "" | Some n -> n) in
	match s with parser
	| [< n , p1 = parse_class_flags; name = opt_name; tl = parse_constraint_params; hl = psep Comma parse_class_herit; '(BrOpen,_); fl, p2 = parse_class_fields false p1 >] ->
		(EClass {
			d_name = name;
			d_doc = doc;
			d_meta = meta;
			d_params = tl;
			d_flags = List.map fst cflags @ n @ hl;
			d_data = fl;
		}, punion p1 p2)

and parse_import s p1 =
	let rec loop acc =
		match s with parser
		| [< '(Dot,p) >] ->
			if is_resuming p then raise (TypePath (List.rev (List.map fst acc),None));
			(match s with parser
			| [< '(Const (Ident k),p) >] ->
				loop ((k,p) :: acc)
			| [< '(Kwd Macro,p) >] ->
				loop (("macro",p) :: acc)
			| [< '(Binop OpMult,_); '(Semicolon,p2) >] ->
				p2, List.rev acc, IAll
			| [< '(Binop OpOr,_) when do_resume() >] ->
				raise (TypePath (List.rev (List.map fst acc),None))
			| [< >] ->
				serror());
		| [< '(Semicolon,p2) >] ->
			p2, List.rev acc, INormal
		| [< '(Kwd In,_); '(Const (Ident name),_); '(Semicolon,p2) >] ->
			p2, List.rev acc, IAsName name
		| [< >] ->
			serror()
	in
	let p2, path, mode = (match s with parser
		| [< '(Const (Ident name),p) >] -> loop [name,p]
		| [< >] -> serror()
	) in
	(EImport (path,mode),punion p1 p2)

and parse_abstract_relations s =
	match s with parser
	| [< '(Const (Ident "to"),_); t = parse_complex_type >] -> AToType t
	| [< '(Const (Ident "from"),_); t = parse_complex_type >] -> AFromType t

and parse_abstract_subtype s =
	match s with parser
	| [< '(POpen, _); t = parse_complex_type; '(PClose,_) >] -> Some t
	| [< >] -> None

and parse_package s = psep Dot lower_ident_or_macro s

and parse_class_fields tdecl p1 s =
	let l = parse_class_field_resume tdecl s in
	let p2 = (match s with parser
		| [< '(BrClose,p2) >] -> p2
		| [< >] -> if do_resume() then p1 else serror()
	) in
	l, p2

and parse_class_field_resume tdecl s =
	if not (do_resume()) then
		plist parse_class_field s
	else try
		let c = parse_class_field s in
		c :: parse_class_field_resume tdecl s
	with Stream.Error _ | Stream.Failure ->
		(* look for next variable/function or next type declaration *)
		let rec junk k =
			if k <= 0 then () else begin
				Stream.junk s;
				junk (k - 1);
			end
		in
		(*
			walk back tokens which are prefixing a type/field declaration
		*)
		let rec junk_tokens k =
			if k = 0 then
				()
			else match List.rev_map fst (Stream.npeek k s) with
			| Kwd Private :: _ -> junk_tokens (k - 1)
			| (Const (Ident _) | Kwd _) :: DblDot :: At :: l
			| (Const (Ident _) | Kwd _) :: At :: l ->
				junk_tokens (List.length l)
			| PClose :: l ->
				(* count matching parenthesises for metadata call *)
				let rec loop n = function
					| [] -> []
					| POpen :: l -> if n = 0 then l else loop (n - 1) l
					| PClose :: l -> loop (n + 1) l
					| _ :: l -> loop n l
				in
				(match loop 0 l with
				| (Const (Ident _) | Kwd _) :: At :: l
				| (Const (Ident _) | Kwd _) :: DblDot :: At :: l -> junk_tokens (List.length l)
				| _ ->
					junk k)
			| _ ->
				junk k
		in
		let rec loop k =
			match List.rev_map fst (Stream.npeek k s) with
			(* field declaration *)
			| Const _ :: Kwd Function :: _
			| Kwd New :: Kwd Function :: _ ->
				junk_tokens (k - 2);
				parse_class_field_resume tdecl s
			| Kwd Macro :: _ | Kwd Public :: _ | Kwd Static :: _ | Kwd Var :: _ | Kwd Override :: _ | Kwd Dynamic :: _ | Kwd Inline :: _ ->
				junk_tokens (k - 1);
				parse_class_field_resume tdecl s
			| BrClose :: _ when tdecl ->
				junk_tokens (k - 1);
				[]
			(* type declaration *)
			| Eof :: _ | Kwd Import :: _ | Kwd Using :: _ | Kwd Extern :: _ | Kwd Class :: _ | Kwd Interface :: _ | Kwd Enum :: _ | Kwd Typedef :: _ | Kwd Abstract :: _->
				junk_tokens (k - 1);
				[]
			| [] ->
				[]
			| _ ->
				loop (k + 1)
		in
		loop 1

and parse_common_flags = parser
	| [< '(Kwd Private,_); l = parse_common_flags >] -> (HPrivate, EPrivate) :: l
	| [< '(Kwd Extern,_); l = parse_common_flags >] -> (HExtern, EExtern) :: l
	| [< >] -> []

and parse_meta_params pname s = match s with parser
	| [< '(POpen,p) when p.pmin = pname.pmax; params = psep Comma expr; '(PClose,_); >] -> params
	| [< >] -> []

and parse_meta_entry = parser
	[< '(At,_); name,p = meta_name; params = parse_meta_params p; s >] -> (name,params,p)

and parse_meta = parser
	| [< entry = parse_meta_entry; s >] ->
		entry :: parse_meta s
	| [< >] -> []

and meta_name = parser
	| [< '(Const (Ident i),p) >] -> (Meta.Custom i), p
	| [< '(Kwd k,p) >] -> (Meta.Custom (s_keyword k)),p
	| [< '(DblDot,_); s >] -> match s with parser
		| [< '(Const (Ident i),p) >] -> (Common.MetaInfo.parse i), p
		| [< '(Kwd k,p) >] -> (Common.MetaInfo.parse (s_keyword k)),p

and parse_enum_flags = parser
	| [< '(Kwd Enum,p) >] -> [] , p

and parse_class_flags = parser
	| [< '(Kwd Class,p) >] -> [] , p
	| [< '(Kwd Interface,p) >] -> [HInterface] , p

and parse_type_opt = parser
	| [< '(DblDot,_); t = parse_complex_type >] -> Some t
	| [< >] -> None

and parse_complex_type s =
	let t = parse_complex_type_inner s in
	parse_complex_type_next t s

and parse_complex_type_inner = parser
	| [< '(POpen,_); t = parse_complex_type; '(PClose,_) >] -> CTParent t
	| [< '(BrOpen,p1); s >] ->
		(match s with parser
		| [< l = parse_type_anonymous false >] -> CTAnonymous l
		| [< '(Binop OpGt,_); t = parse_type_path; '(Comma,_); s >] ->
			(match s with parser
			| [< l = parse_type_anonymous false >] -> CTExtend (t,l)
			| [< l, _ = parse_class_fields true p1 >] -> CTExtend (t,l)
			| [< >] -> serror())
		| [< l, _ = parse_class_fields true p1 >] -> CTAnonymous l
		| [< >] -> serror())
	| [< '(Question,_); t = parse_complex_type_inner >] ->
		CTOptional t
	| [< t = parse_type_path >] ->
		CTPath t

and parse_type_path s = parse_type_path1 [] s

and parse_type_path1 pack = parser
	| [< name, p = dollar_ident_macro pack; s >] ->
		if is_lower_ident name then
			(match s with parser
			| [< '(Dot,p) >] ->
				if is_resuming p then
					raise (TypePath (List.rev (name :: pack),None))
				else
					parse_type_path1 (name :: pack) s
			| [< '(Semicolon,_) >] ->
				error (Custom "Type name should start with an uppercase letter") p
			| [< >] -> serror())
		else
			let sub = (match s with parser
				| [< '(Dot,p); s >] ->
					(if is_resuming p then
						raise (TypePath (List.rev pack,Some (name,false)))
					else match s with parser
						| [< '(Const (Ident name),_) when not (is_lower_ident name) >] -> Some name
						| [< '(Binop OpOr,_) when do_resume() >] ->
							raise (TypePath (List.rev pack,Some (name,false)))
						| [< >] -> serror())
				| [< >] -> None
			) in
			let params = (match s with parser
				| [< '(Binop OpLt,_); l = psep Comma parse_type_path_or_const; '(Binop OpGt,_) >] -> l
				| [< >] -> []
			) in
			{
				tpackage = List.rev pack;
				tname = name;
				tparams = params;
				tsub = sub;
			}
	| [< '(Binop OpOr,_) when do_resume() >] ->
		raise (TypePath (List.rev pack,None))

and type_name = parser
	| [< '(Const (Ident name),p) >] ->
		if is_lower_ident name then
			error (Custom "Type name should start with an uppercase letter") p
		else
			name

and parse_type_path_or_const = parser
	(* we can't allow (expr) here *)
	| [< '(BkOpen,p1); l = parse_array_decl; '(BkClose,p2); s >] -> TPExpr (EArrayDecl l, punion p1 p2)
	| [< t = parse_complex_type >] -> TPType t
	| [< '(Const c,p) >] -> TPExpr (EConst c,p)
	| [< e = expr >] -> TPExpr e
	| [< >] -> serror()

and parse_complex_type_next t = parser
	| [< '(Arrow,_); t2 = parse_complex_type >] ->
		(match t2 with
		| CTFunction (args,r) ->
			CTFunction (t :: args,r)
		| _ ->
			CTFunction ([t] , t2))
	| [< >] -> t

and parse_type_anonymous opt = parser
	| [< '(Question,_) when not opt; s >] -> parse_type_anonymous true s
	| [< name, p1 = ident; '(DblDot,_); t = parse_complex_type; s >] ->
		let next p2 acc =
			let t = if not opt then t else (match t with
				| CTPath { tpackage = []; tname = "Null" } -> t
				| _ -> CTPath { tpackage = []; tname = "Null"; tsub = None; tparams = [TPType t] }
			) in
			{
				cff_name = name;
				cff_meta = if opt then [Meta.Optional,[],p1] else [];
				cff_access = [];
				cff_doc = None;
				cff_kind = FVar (Some t,None);
				cff_pos = punion p1 p2;
			} :: acc
		in
		match s with parser
		| [< '(BrClose,p2) >] -> next p2 []
		| [< '(Comma,p2) >] ->
			(match s with parser
			| [< '(BrClose,_) >] -> next p2 []
			| [< l = parse_type_anonymous false >] -> next p2 l
			| [< >] -> serror());
		| [< >] -> serror()

and parse_enum s =
	let doc = get_doc s in
	let meta = parse_meta s in
	match s with parser
	| [< name, p1 = any_enum_ident; params = parse_constraint_params; s >] ->
		let args = (match s with parser
		| [< '(POpen,_); l = psep Comma parse_enum_param; '(PClose,_) >] -> l
		| [< >] -> []
		) in
		let t = (match s with parser
		| [< '(DblDot,_); t = parse_complex_type >] -> Some t
		| [< >] -> None
		) in
		let p2 = (match s with parser
			| [< p = semicolon >] -> p
			| [< >] -> serror()
		) in
		{
			ec_name = name;
			ec_doc = doc;
			ec_meta = meta;
			ec_args = args;
			ec_params = params;
			ec_type = t;
			ec_pos = punion p1 p2;
		}

and parse_enum_param = parser
	| [< '(Question,_); name, _ = ident; '(DblDot,_); t = parse_complex_type >] -> (name,true,t)
	| [< name, _ = ident; '(DblDot,_); t = parse_complex_type >] -> (name,false,t)

and parse_class_field s =
	let doc = get_doc s in
	match s with parser
	| [< meta = parse_meta; al = parse_cf_rights true []; s >] ->
		let name, pos, k = (match s with parser
		| [< '(Kwd Var,p1); name, _ = ident; s >] ->
			(match s with parser
			| [< '(POpen,_); i1 = property_ident; '(Comma,_); i2 = property_ident; '(PClose,_) >] ->
				let t = (match s with parser
					| [< '(DblDot,_); t = parse_complex_type >] -> Some t
					| [< >] -> None
				) in
				let e , p2 = (match s with parser
				| [< '(Binop OpAssign,_); e = toplevel_expr; p2 = semicolon >] -> Some e , p2
				| [< '(Semicolon,p2) >] -> None , p2
				| [< >] -> serror()
				) in
				name, punion p1 p2, FProp (i1,i2,t, e)
			| [< t = parse_type_opt; s >] ->
				let e , p2 = (match s with parser
				| [< '(Binop OpAssign,_); e = toplevel_expr; p2 = semicolon >] -> Some e , p2
				| [< '(Semicolon,p2) >] -> None , p2
				| [< >] -> serror()
				) in
				name, punion p1 p2, FVar (t,e))
		| [< '(Kwd Function,p1); name = parse_fun_name; pl = parse_constraint_params; '(POpen,_); al = psep Comma parse_fun_param; '(PClose,_); t = parse_type_opt; s >] ->
			let e, p2 = (match s with parser
				| [< e = toplevel_expr; s >] ->
					(try ignore(semicolon s) with Error (Missing_semicolon,p) -> !display_error Missing_semicolon p);
					Some e, pos e
				| [< '(Semicolon,p) >] -> None, p
				| [< >] -> serror()
			) in
			let f = {
				f_params = pl;
				f_args = al;
				f_type = t;
				f_expr = e;
			} in
			name, punion p1 p2, FFun f
		| [< >] ->
			if al = [] then raise Stream.Failure else serror()
		) in
		{
			cff_name = name;
			cff_doc = doc;
			cff_meta = meta;
			cff_access = al;
			cff_pos = pos;
			cff_kind = k;
		}

and parse_cf_rights allow_static l = parser
	| [< '(Kwd Static,_) when allow_static; l = parse_cf_rights false (AStatic :: l) >] -> l
	| [< '(Kwd Macro,_) when not(List.mem AMacro l); l = parse_cf_rights allow_static (AMacro :: l) >] -> l
	| [< '(Kwd Public,_) when not(List.mem APublic l || List.mem APrivate l); l = parse_cf_rights allow_static (APublic :: l) >] -> l
	| [< '(Kwd Private,_) when not(List.mem APublic l || List.mem APrivate l); l = parse_cf_rights allow_static (APrivate :: l) >] -> l
	| [< '(Kwd Override,_) when not (List.mem AOverride l); l = parse_cf_rights false (AOverride :: l) >] -> l
	| [< '(Kwd Dynamic,_) when not (List.mem ADynamic l); l = parse_cf_rights allow_static (ADynamic :: l) >] -> l
	| [< '(Kwd Inline,_); l = parse_cf_rights allow_static (AInline :: l) >] -> l
	| [< >] -> l

and parse_fun_name = parser
	| [< '(Const (Ident name),_) >] -> name
	| [< '(Kwd New,_) >] -> "new"

and parse_fun_param = parser
	| [< '(Question,_); name, _ = ident; t = parse_type_opt; c = parse_fun_param_value >] -> (name,true,t,c)
	| [< name, _ = ident; t = parse_type_opt; c = parse_fun_param_value >] -> (name,false,t,c)

and parse_fun_param_value = parser
	| [< '(Binop OpAssign,_); e = toplevel_expr >] -> Some e
	| [< >] -> None

and parse_fun_param_type = parser
	| [< '(Question,_); name = ident; '(DblDot,_); t = parse_complex_type >] -> (name,true,t)
	| [< name = ident; '(DblDot,_); t = parse_complex_type >] -> (name,false,t)

and parse_constraint_params = parser
	| [< '(Binop OpLt,_); l = psep Comma parse_constraint_param; '(Binop OpGt,_) >] -> l
	| [< >] -> []

and parse_constraint_param = parser
	| [< name = type_name; s >] ->
		let params = (match s with parser
			| [< >] -> []
		) in
		let ctl = (match s with parser
			| [< '(DblDot,_); s >] ->
				(match s with parser
				| [< '(POpen,_); l = psep Comma parse_complex_type; '(PClose,_) >] -> l
				| [< t = parse_complex_type >] -> [t]
				| [< >] -> serror())
			| [< >] -> []
		) in
		{
			tp_name = name;
			tp_params = params;
			tp_constraints = ctl;
		}

and parse_class_herit = parser
	| [< '(Kwd Extends,_); t = parse_type_path >] -> HExtends t
	| [< '(Kwd Implements,_); t = parse_type_path >] -> HImplements t

and block1 = parser
	| [< '(Const (Ident name),p); s >] -> block2 name (Ident name) p s
	| [< '(Const (String name),p); s >] -> block2 (quote_ident name) (String name) p s
	| [< b = block [] >] -> EBlock b

and block2 name ident p s =
	match s with parser
	| [< '(DblDot,_); e = expr; l = parse_obj_decl >] -> EObjectDecl ((name,e) :: l)
	| [< >] ->
		let e = expr_next (EConst ident,p) s in
		try
			let _ = semicolon s in
			let b = block [e] s in
			EBlock b
		with
			| Error (err,p) ->
				(!display_error) err p;
				EBlock (block [e] s)

and block acc s =
	try
		(* because of inner recursion, we can't put Display handling in errors below *)
		let e = try parse_block_elt s with Display e -> display (EBlock (List.rev (e :: acc)),snd e) in
		block (e :: acc) s
	with
		| Stream.Failure ->
			List.rev acc
		| Stream.Error _ ->
			let tk , pos = (match Stream.peek s with None -> last_token s | Some t -> t) in
			(!display_error) (Unexpected tk) pos;
			block acc s
        | Error (e,p) ->
			(!display_error) e p;
			block acc s

and parse_block_elt = parser
	| [< '(Kwd Var,p1); vl = psep Comma parse_var_decl; p2 = semicolon >] -> (EVars vl,punion p1 p2)
	| [< e = expr; _ = semicolon >] -> e

and parse_obj_decl = parser
	| [< '(Comma,_); s >] ->
		(match s with parser
		| [< name, _ = ident; '(DblDot,_); e = expr; l = parse_obj_decl >] -> (name,e) :: l
		| [< '(Const (String name),_); '(DblDot,_); e = expr; l = parse_obj_decl >] -> (quote_ident name,e) :: l
		| [< >] -> [])
	| [< >] -> []

and parse_array_decl = parser
	| [< e = expr; s >] ->
		(match s with parser
		| [< '(Comma,_); l = parse_array_decl >] -> e :: l
		| [< >] -> [e])
	| [< >] ->
		[]

and parse_var_decl = parser
	| [< name, _ = dollar_ident; t = parse_type_opt; s >] ->
		match s with parser
		| [< '(Binop OpAssign,_); e = expr >] -> (name,t,Some e)
		| [< >] -> (name,t,None)

and inline_function = parser
	| [< '(Kwd Inline,_); '(Kwd Function,p1) >] -> true, p1
	| [< '(Kwd Function,p1) >] -> false, p1

and reify_expr e =
	let to_expr,_,_ = reify !in_macro in
	let e = to_expr e in
	(ECheckType (e,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = None; tparams = [] })),pos e)

and parse_macro_expr p = parser
	| [< '(DblDot,_); t = parse_complex_type >] ->
		let _, to_type, _  = reify !in_macro in
		let t = to_type t p in
		(ECheckType (t,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some "ComplexType"; tparams = [] })),p)
	| [< '(Kwd Var,p1); vl = psep Comma parse_var_decl >] ->
		reify_expr (EVars vl,p1)
	| [< d = parse_class None [] [] false >] ->
		let _,_,to_type = reify !in_macro in
		(ECheckType (to_type d,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some "TypeDefinition"; tparams = [] })),p)
	| [< e = secure_expr >] ->
		reify_expr e

and expr = parser
	| [< (name,params,p) = parse_meta_entry; s >] ->
		(try
			make_meta name params (secure_expr s) p
		with Display e ->
			display (make_meta name params e p))
	| [< '(BrOpen,p1); b = block1; '(BrClose,p2); s >] ->
		let e = (b,punion p1 p2) in
		(match b with
		| EObjectDecl _ -> expr_next e s
		| _ -> e)
	| [< '(Kwd Macro,p); s >] ->
		parse_macro_expr p s
	| [< '(Kwd Var,p1); v = parse_var_decl >] -> (EVars [v],p1)
	| [< '(Const c,p); s >] -> expr_next (EConst c,p) s
	| [< '(Kwd This,p); s >] -> expr_next (EConst (Ident "this"),p) s
	| [< '(Kwd True,p); s >] -> expr_next (EConst (Ident "true"),p) s
	| [< '(Kwd False,p); s >] -> expr_next (EConst (Ident "false"),p) s
	| [< '(Kwd Null,p); s >] -> expr_next (EConst (Ident "null"),p) s
	| [< '(Kwd Cast,p1); s >] ->
		(match s with parser
		| [< '(POpen,_); e = expr; s >] ->
			(match s with parser
			| [< '(Comma,_); t = parse_complex_type; '(PClose,p2); s >] -> expr_next (ECast (e,Some t),punion p1 p2) s
			| [< '(PClose,p2); s >] -> expr_next (ECast (e,None),punion p1 (pos e)) s
			| [< >] -> serror())
		| [< e = secure_expr >] -> expr_next (ECast (e,None),punion p1 (pos e)) s)
	| [< '(Kwd Throw,p); e = expr >] -> (EThrow e,p)
	| [< '(Kwd New,p1); t = parse_type_path; '(POpen,p); s >] ->
		if is_resuming p then display (EDisplayNew t,punion p1 p);
		(match s with parser
		| [< al = psep Comma expr; '(PClose,p2); s >] -> expr_next (ENew (t,al),punion p1 p2) s
		| [< >] -> serror())
	| [< '(POpen,p1); e = expr; '(PClose,p2); s >] -> expr_next (EParenthesis e, punion p1 p2) s
	| [< '(BkOpen,p1); l = parse_array_decl; '(BkClose,p2); s >] -> expr_next (EArrayDecl l, punion p1 p2) s
	| [< inl, p1 = inline_function; name = popt dollar_ident; pl = parse_constraint_params; '(POpen,_); al = psep Comma parse_fun_param; '(PClose,_); t = parse_type_opt; s >] ->
		let make e =
			let f = {
				f_params = pl;
				f_type = t;
				f_args = al;
				f_expr = Some e;
			} in
			EFunction ((match name with None -> None | Some (name,_) -> Some (if inl then "inline_" ^ name else name)),f), punion p1 (pos e)
		in
		(try
			expr_next (make (secure_expr s)) s
		with
			Display e -> display (make e))
	| [< '(Unop op,p1) when is_prefix op; e = expr >] -> make_unop op e p1
	| [< '(Binop OpSub,p1); e = expr >] ->
		let neg s =
			if s.[0] = '-' then String.sub s 1 (String.length s - 1) else "-" ^ s
		in
		(match make_unop Neg e p1 with
		| EUnop (Neg,Prefix,(EConst (Int i),pc)),p -> EConst (Int (neg i)),p
		| EUnop (Neg,Prefix,(EConst (Float j),pc)),p -> EConst (Float (neg j)),p
		| e -> e)
	(*/* removed unary + : this cause too much syntax errors go unnoticed, such as "a + + 1" (missing 'b')
						without adding anything to the language
	| [< '(Binop OpAdd,p1); s >] ->
		(match s with parser
		| [< '(Const (Int i),p); e = expr_next (EConst (Int i),p) >] -> e
		| [< '(Const (Float f),p); e = expr_next (EConst (Float f),p) >] -> e
		| [< >] -> serror()) */*)
	| [< '(Kwd For,p); '(POpen,_); it = expr; '(PClose,_); s >] ->
		(try
			let e = secure_expr s in
			(EFor (it,e),punion p (pos e))
		with
			Display e -> display (EFor (it,e),punion p (pos e)))
	| [< '(Kwd If,p); '(POpen,_); cond = expr; '(PClose,_); e1 = expr; s >] ->
		let e2 = (match s with parser
			| [< '(Kwd Else,_); e2 = expr; s >] -> Some e2
			| [< >] ->
				match Stream.npeek 2 s with
				| [(Semicolon,_); (Kwd Else,_)] ->
					Stream.junk s;
					Stream.junk s;
					Some (secure_expr s)
				| _ ->
					None
		) in
		(EIf (cond,e1,e2), punion p (match e2 with None -> pos e1 | Some e -> pos e))
	| [< '(Kwd Return,p); e = popt expr >] -> (EReturn e, match e with None -> p | Some e -> punion p (pos e))
	| [< '(Kwd Break,p) >] -> (EBreak,p)
	| [< '(Kwd Continue,p) >] -> (EContinue,p)
	| [< '(Kwd While,p1); '(POpen,_); cond = expr; '(PClose,_); s >] ->
		(try
			let e = secure_expr s in
			(EWhile (cond,e,NormalWhile),punion p1 (pos e))
		with
			Display e -> display (EWhile (cond,e,NormalWhile),punion p1 (pos e)))
	| [< '(Kwd Do,p1); e = expr; '(Kwd While,_); '(POpen,_); cond = expr; '(PClose,_); s >] -> (EWhile (cond,e,DoWhile),punion p1 (pos e))
	| [< '(Kwd Switch,p1); e = expr; '(BrOpen,_); cases , def = parse_switch_cases e []; '(BrClose,p2); s >] -> (ESwitch (e,cases,def),punion p1 p2)
	| [< '(Kwd Try,p1); e = expr; cl = plist (parse_catch e); s >] -> (ETry (e,cl),p1)
	| [< '(IntInterval i,p1); e2 = expr >] -> make_binop OpInterval (EConst (Int i),p1) e2
	| [< '(Kwd Untyped,p1); e = expr >] -> (EUntyped e,punion p1 (pos e))
	| [< '(Dollar v,p); s >] -> expr_next (EConst (Ident ("$"^v)),p) s

and expr_next e1 = parser
 	| [< '(BrOpen,p1) when is_dollar_ident e1; eparam = expr; '(BrClose,p2); s >] ->
		(match fst e1 with
		| EConst(Ident n) -> expr_next (EMeta((Common.MetaInfo.from_string n,[],snd e1),eparam), punion p1 p2) s
		| _ -> assert false)
	| [< '(Dot,p); s >] ->
		if is_resuming p then display (EDisplay (e1,false),p);
		(match s with parser
		| [< '(Kwd Macro,p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,"macro") , punion (pos e1) p2) s
		| [< '(Const (Ident f),p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,f) , punion (pos e1) p2) s
		| [< '(Dollar v,p2); s >] -> expr_next (EField (e1,"$"^v) , punion (pos e1) p2) s
		| [< '(Binop OpOr,p2) when do_resume() >] -> display (EDisplay (e1,false),p) (* help for debug display mode *)
		| [< >] ->
			(* turn an integer followed by a dot into a float *)
			match e1 with
			| (EConst (Int v),p2) when p2.pmax = p.pmin -> expr_next (EConst (Float (v ^ ".")),punion p p2) s
			| _ -> serror())
	| [< '(POpen,p1); s >] ->
		if is_resuming p1 then display (EDisplay (e1,true),p1);
		(match s with parser
		| [< '(Binop OpOr,p2) when do_resume() >] -> display (EDisplay (e1,true),p1) (* help for debug display mode *)
		| [< params = parse_call_params e1; '(PClose,p2); s >] -> expr_next (ECall (e1,params) , punion (pos e1) p2) s
		| [< >] -> serror())
	| [< '(BkOpen,_); e2 = expr; '(BkClose,p2); s >] ->
		expr_next (EArray (e1,e2), punion (pos e1) p2) s
	| [< '(Binop OpGt,_); s >] ->
		(match s with parser
		| [< '(Binop OpGt,_); s >] ->
			(match s with parser
			| [< '(Binop OpGt,_) >] ->
				(match s with parser
				| [< '(Binop OpAssign,_); e2 = expr >] -> make_binop (OpAssignOp OpUShr) e1 e2
				| [< e2 = secure_expr >] -> make_binop OpUShr e1 e2)
			| [< '(Binop OpAssign,_); e2 = expr >] -> make_binop (OpAssignOp OpShr) e1 e2
			| [< e2 = secure_expr >] -> make_binop OpShr e1 e2)
		| [< '(Binop OpAssign,_); s >] ->
			make_binop OpGte e1 (secure_expr s)
		| [< e2 = secure_expr >] ->
			make_binop OpGt e1 e2)
	| [< '(Binop op,_); e2 = expr >] ->
		make_binop op e1 e2
	| [< '(Unop op,p) when is_postfix e1 op; s >] ->
		expr_next (EUnop (op,Postfix,e1), punion (pos e1) p) s
	| [< '(Question,_); e2 = expr; '(DblDot,_); e3 = expr >] ->
		(ETernary (e1,e2,e3),punion (pos e1) (pos e3))
	| [< '(Kwd In,_); e2 = expr >] ->
		(EIn (e1,e2), punion (pos e1) (pos e2))
	| [< >] -> e1

and parse_guard = parser
	| [< '(Kwd If,p1); '(POpen,_); e = expr; '(PClose,_); >] ->
		e

and parse_switch_cases eswitch cases = parser
	| [< '(Kwd Default,p1); '(DblDot,_); s >] ->
		let b = (try block [] s with Display e -> display (ESwitch (eswitch,cases,Some (Some e)),punion (pos eswitch) (pos e))) in
		let b = match b with
			| [] -> None
			| _ -> Some ((EBlock b,p1))
		in
		let l , def = parse_switch_cases eswitch cases s in
		(match def with None -> () | Some _ -> error Duplicate_default p1);
		l , Some b
	| [< '(Kwd Case,p1); el = psep Comma expr; eg = popt parse_guard; '(DblDot,_); s >] ->
		let b = (try block [] s with Display e -> display (ESwitch (eswitch,List.rev ((el,eg,Some e) :: cases),None),punion (pos eswitch) (pos e))) in
		let b = match b with
			| [] -> None
			| _ -> Some ((EBlock b,p1))
		in
		parse_switch_cases eswitch ((el,eg,b) :: cases) s
	| [< >] ->
		List.rev cases , None

and parse_catch etry = parser
	| [< '(Kwd Catch,p); '(POpen,_); name, _ = ident; s >] ->
		match s with parser
		| [< '(DblDot,_); t = parse_complex_type; '(PClose,_); s >] ->
			(try
				(name,t,secure_expr s)
			with
				Display e -> display (ETry (etry,[name,t,e]),punion (pos etry) (pos e)))
		| [< '(_,p) >] -> error Missing_type p

and parse_call_params ec s =
	let e = (try
		match s with parser
		| [< e = expr >] -> Some e
		| [< >] -> None
	with Display e ->
		display (ECall (ec,[e]),punion (pos ec) (pos e))
	) in
	let rec loop acc =
		try
			match s with parser
			| [< '(Comma,_); e = expr >] -> loop (e::acc)
			| [< >] -> List.rev acc
		with Display e ->
			display (ECall (ec,List.rev (e::acc)),punion (pos ec) (pos e))
	in
	match e with
	| None -> []
	| Some e -> loop [e]

and parse_macro_cond allow_op s =
	match s with parser
	| [< '(Const (Ident t),p) >] ->
		parse_macro_ident allow_op t p s
	| [< '(Const (String s),p) >] ->
		None, (EConst (String s),p)
	| [< '(Const (Int i),p) >] ->
		None, (EConst (Int i),p)
	| [< '(Const (Float f),p) >] ->
		None, (EConst (Float f),p)
	| [< '(Kwd k,p) >] ->
		parse_macro_ident allow_op (s_keyword k) p s
	| [< '(POpen, p1); _,e = parse_macro_cond true; '(PClose, p2) >] ->
		let e = (EParenthesis e,punion p1 p2) in
		if allow_op then parse_macro_op e s else None, e
	| [< '(Unop op,p); tk, e = parse_macro_cond allow_op >] ->
		tk, make_unop op e p

and parse_macro_ident allow_op t p s =
	let e = (EConst (Ident t),p) in
	if not allow_op then
		None, e
	else
		parse_macro_op e s

and parse_macro_op e s =
	match Stream.peek s with
	| Some (Binop op,_) ->
		Stream.junk s;
		let op = match Stream.peek s with
			| Some (Binop OpAssign,_) when op = OpGt ->
				Stream.junk s;
				OpGte
			| _ -> op
		in
		let tk, e2 = (try parse_macro_cond true s with Stream.Failure -> serror()) in
		tk, make_binop op e e2
	| tk ->
		tk, e

and toplevel_expr s =
	try
		expr s
	with
		Display e -> e

and secure_expr s =
	match s with parser
	| [< e = expr >] -> e
	| [< >] -> serror()

(* eval *)
type small_type =
	| TNull
	| TBool of bool
	| TFloat of float
	| TString of string

let is_true = function
	| TBool false | TNull | TFloat 0. | TString "" -> false
	| _ -> true

let cmp v1 v2 =
	match v1, v2 with
	| TNull, TNull -> 0
	| TFloat a, TFloat b -> compare a b
	| TString a, TString b -> compare a b
	| TBool a, TBool b -> compare a b
	| TString a, TFloat b -> compare (float_of_string a) b
	| TFloat a, TString b -> compare a (float_of_string b)
	| _ -> raise Exit (* always false *)

let rec eval ctx (e,p) =
	match e with
	| EConst (Ident i) ->
		(try TString (Common.raw_defined_value ctx i) with Not_found -> TNull)
	| EConst (String s) -> TString s
	| EConst (Int i) -> TFloat (float_of_string i)
	| EConst (Float f) -> TFloat (float_of_string f)
	| EBinop (OpBoolAnd, e1, e2) -> TBool (is_true (eval ctx e1) && is_true (eval ctx e2))
	| EBinop (OpBoolOr, e1, e2) -> TBool (is_true (eval ctx e1) || is_true(eval ctx e2))
	| EUnop (Not, _, e) -> TBool (not (is_true (eval ctx e)))
	| EParenthesis e -> eval ctx e
	| EBinop (op, e1, e2) ->
		let v1 = eval ctx e1 in
		let v2 = eval ctx e2 in
		let compare op =
			TBool (try op (cmp v1 v2) 0 with _ -> false)
		in
		(match op with
		| OpEq -> compare (=)
		| OpNotEq -> compare (<>)
		| OpGt -> compare (>)
		| OpGte -> compare (>=)
		| OpLt -> compare (<)
		| OpLte -> compare (<=)
		| _ -> error (Custom "Unsupported operation") p)
	| _ ->
		error (Custom "Invalid condition expression") p

(* parse main *)
let parse ctx code =
	let old = Lexer.save() in
	let old_cache = !cache in
	let mstack = ref [] in
	cache := DynArray.create();
	last_doc := None;
	in_macro := Common.defined ctx Common.Define.Macro;
	Lexer.skip_header code;

	let sraw = Stream.from (fun _ -> Some (Lexer.token code)) in
	let rec next_token() = process_token (Lexer.token code)

	and process_token tk =
		match fst tk with
		| Comment s ->
			let tk = next_token() in
			if !use_doc then begin
				let l = String.length s in
				if l > 0 && s.[0] = '*' then last_doc := Some (String.sub s 1 (l - (if l > 1 && s.[l-1] = '*' then 2 else 1)), (snd tk).pmin);
			end;
			tk
		| CommentLine s ->
			next_token()
		| Sharp "end" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				mstack := l;
				next_token())
		| Sharp "else" | Sharp "elseif" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				mstack := l;
				process_token (skip_tokens (snd tk) false))
		| Sharp "if" ->
			process_token (enter_macro (snd tk))
		| Sharp "error" ->
			(match Lexer.token code with
			| (Const (String s),p) -> error (Custom s) p
			| _ -> error Unimplemented (snd tk))
		| Sharp "line" ->
			let line = (match next_token() with
				| (Const (Int s),_) -> int_of_string s
				| (t,p) -> error (Unexpected t) p
			) in
			!(Lexer.cur).Lexer.lline <- line - 1;
			next_token();
		| _ ->
			tk

	and enter_macro p =
		let tk, e = parse_macro_cond false sraw in
		let tk = (match tk with None -> Lexer.token code | Some tk -> tk) in
		if is_true (eval ctx e) || (match fst e with EConst (Ident "macro") when Common.unique_full_path p.pfile = (!resume_display).pfile -> true | _ -> false) then begin
			mstack := p :: !mstack;
			tk
		end else
			skip_tokens_loop p true tk

	and skip_tokens_loop p test tk =
		match fst tk with
		| Sharp "end" ->
			Lexer.token code
		| Sharp "elseif" | Sharp "else" when not test ->
			skip_tokens p test
		| Sharp "else" ->
			mstack := snd tk :: !mstack;
			Lexer.token code
		| Sharp "elseif" ->
			enter_macro (snd tk)
		| Sharp "if" ->
			skip_tokens_loop p test (skip_tokens p false)
		| Eof ->
			if do_resume() then tk else error Unclosed_macro p
		| _ ->
			skip_tokens p test

	and skip_tokens p test = skip_tokens_loop p test (Lexer.token code)

	in
	let s = Stream.from (fun _ ->
		let t = next_token() in
		DynArray.add (!cache) t;
		Some t
	) in
	try
		let l = parse_file s in
		(match !mstack with p :: _ when not (do_resume()) -> error Unclosed_macro p | _ -> ());
		cache := old_cache;
		Lexer.restore old;
		l
	with
		| Stream.Error _
		| Stream.Failure ->
			let last = (match Stream.peek s with None -> last_token s | Some t -> t) in
			Lexer.restore old;
			cache := old_cache;
			error (Unexpected (fst last)) (pos last)
		| e ->
			Lexer.restore old;
			cache := old_cache;
			raise e
