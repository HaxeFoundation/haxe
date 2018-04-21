(*
	The Haxe Compiler
	Copyright (C) 2005-2018  Haxe Foundation

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
open Ast
open Globals

let reify in_macro =
	let cur_pos = ref None in
	let mk_enum ename n vl p =
		(* We don't want the position of the call expression to span the entire call (#6396). *)
		let pmin = {p with pmax = p.pmin} in
		let constr = (EConst (Ident n),pmin) in
		match vl with
		| [] -> constr
		| _ -> (ECall (constr,vl),pmin)
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
		| OpIn -> op "OpIn"
	in
	let to_string s p =
		let len = String.length s in
		if len > 1 && s.[0] = '$' then
			(EConst (Ident (String.sub s 1 (len - 1))),p)
		else
			(EConst (String s),p)
	in
	let to_placed_name (s,p) =
		to_string s p
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
		(EObjectDecl (List.map (fun (s,e) -> (s,null_pos,NoQuotes),e) fields),p)
	in
	let rec to_tparam t p =
		let n, v = (match t with
			| TPType t -> "TPType", to_ctype t p
			| TPExpr e -> "TPExpr", to_expr e p
		) in
		mk_enum "TypeParam" n [v] p
	and to_tpath (t,_) p =
		let len = String.length t.tname in
		if t.tpackage = [] && len > 1 && t.tname.[0] = '$' then begin
			let name = String.sub t.tname 1 (len - 1) in
			let ei = (EConst (Ident name),p) in
			match t.tparams with
			| [] -> ei
			| _ ->
				(* `macro : $TP<Int>` conveys the intent to use TP and overwrite the
					 type parameters. *)
				let ea = to_array to_tparam t.tparams p in
				let fields = [
					("pack", (EField(ei,"pack"),p));
					("name", (EField(ei,"name"),p));
					("sub", (EField(ei,"sub"),p));
					("params", ea);
				] in
				to_obj fields p
		end else begin
			let fields = [
				("pack", to_array to_string t.tpackage p);
				("name", to_string t.tname p);
				("params", to_array to_tparam t.tparams p);
			] in
			to_obj (match t.tsub with None -> fields | Some s -> fields @ ["sub",to_string s p]) p
		end
	and to_ctype t p =
		let ct n vl = mk_enum "ComplexType" n vl p in
		match fst t with
		| CTPath ({ tpackage = []; tparams = []; tsub = None; tname = n }) when n.[0] = '$' ->
			to_string n p
		| CTPath t -> ct "TPath" [to_tpath (t,p) p]
		| CTFunction (args,ret) -> ct "TFunction" [to_array to_type_hint args p; to_type_hint ret p]
		| CTAnonymous fields -> ct "TAnonymous" [to_array to_cfield fields p]
		| CTParent t -> ct "TParent" [to_type_hint t p]
		| CTExtend (tl,fields) -> ct "TExtend" [to_array to_tpath tl p; to_array to_cfield fields p]
		| CTOptional t -> ct "TOptional" [to_type_hint t p]
		| CTNamed (n,t) -> ct "TNamed" [to_placed_name n; to_type_hint t p]
	and to_type_hint (t,p) _ =
		(* to_obj ["type",to_ctype t p;"pos",to_pos p] p *)
		to_ctype (t,p) p
	and to_fun f p =
		let p = {p with pmax = p.pmin} in
		let farg ((n,_),o,_,t,e) p =
			let fields = [
				"name", to_string n p;
				"opt", to_bool o p;
				"type", to_opt to_type_hint t p;
			] in
			to_obj (match e with None -> fields | Some e -> fields @ ["value",to_expr e p]) p
		in
		let rec fparam t p =
			let fields = [
				"name", to_placed_name t.tp_name;
				"constraints", to_array to_ctype t.tp_constraints p;
				"params", to_array fparam t.tp_params p;
			] in
			to_obj fields p
		in
		let fields = [
			("args",to_array farg f.f_args p);
			("ret",to_opt to_type_hint f.f_type p);
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
			| AFinal -> "AFinal"
			) in
			mk_enum "Access" n [] p
		in
		let to_kind k =
			let n, vl = (match k with
				| FVar (ct,e) -> "FVar", [to_opt to_type_hint ct p;to_opt to_expr e p]
				| FFun f -> "FFun", [to_fun f p]
				| FProp (get,set,t,e) -> "FProp", [to_placed_name get; to_placed_name set; to_opt to_type_hint t p; to_opt to_expr e p]
			) in
			mk_enum "FieldType" n vl p
		in
		let fields = [
			Some ("name", to_placed_name f.cff_name);
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
				"name", to_string (Meta.to_string m) p;
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
			(EUntyped (ECall ((EConst (Ident "$__mk_pos__"),p),[file;pmin;pmax]),p),p)
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
		| EConst (Ident n) when n.[0] = '$' && String.length n > 1 && n <> "$__mk_pos__" ->
			to_string n p
		| EConst c ->
			expr "EConst" [to_const c p]
		| EArray (e1,e2) ->
			expr "EArray" [loop e1;loop e2]
		| EBinop (op,e1,e2) ->
			expr "EBinop" [to_binop op p; loop e1; loop e2]
		| EField (e,s) ->
			let p = {p with pmin = p.pmax - String.length s} in
			expr "EField" [loop e; to_string s p]
		| EParenthesis e ->
			expr "EParenthesis" [loop e]
		| EObjectDecl fl ->
			let quote_kind kk p =
				let n = match kk with
					| NoQuotes -> "Unquoted"
					| DoubleQuotes -> "Quoted"
				in
				mk_enum "QuoteStatus" n [] p
			in
			expr "EObjectDecl" [to_array (fun ((f,pn,kk),e) -> to_obj [("field",to_string f p);("expr",loop e);("quotes",quote_kind kk pn)]) fl p]
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
			expr "EVars" [to_array (fun ((n,pn),th,e) p ->
				let fields = [
					(* "name", to_obj ["name",to_string n pn;"pos",to_pos pn] p; *)
					"name", to_string n pn;
					"type", to_opt to_type_hint th p;
					"expr", to_opt to_expr e p;
				] in
				to_obj fields p
			) vl p]
		| EFunction (name,f) ->
			let name = match name with
				| None ->
					to_null null_pos
				| Some name ->
					if ExtString.String.starts_with name "inline_$" then begin
						let real_name = (String.sub name 7 (String.length name - 7)) in
						let e_name = to_string real_name p in
						let e_inline = to_string "inline_" p in
						let e_add = (EBinop(OpAdd,e_inline,e_name),p) in
						e_add
					end else
						to_string name p
			in
			expr "EFunction" [name; to_fun f p]
		| EBlock el ->
			expr "EBlock" [to_expr_array el p]
		| EFor (e1,e2) ->
			expr "EFor" [loop e1;loop e2]
		| EIf (e1,e2,eelse) ->
			expr "EIf" [loop e1;loop e2;to_opt to_expr eelse p]
		| EWhile (e1,e2,flag) ->
			expr "EWhile" [loop e1;loop e2;to_bool (flag = NormalWhile) p]
		| ESwitch (e1,cases,def) ->
			let scase (el,eg,e,_) p =
				to_obj [("values",to_expr_array el p);"guard",to_opt to_expr eg p;"expr",to_opt to_expr e p] p
			in
			expr "ESwitch" [loop e1;to_array scase cases p;to_opt (fun (e,_) -> to_opt to_expr e) def p]
		| ETry (e1,catches) ->
			let scatch ((n,_),t,e,_) p =
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
			expr "ECast" [loop e; to_opt to_type_hint ct p]
		| EComplexType ct ->
			expr "EComplexType" [to_type_hint (ct,p) p]
		| EDisplay (e,flag) ->
			expr "EDisplay" [loop e; to_bool flag p]
		| EDisplayNew t ->
			expr "EDisplayNew" [to_tpath t p]
		| ETernary (e1,e2,e3) ->
			expr "ETernary" [loop e1;loop e2;loop e3]
		| ECheckType (e1,ct) ->
			expr "ECheckType" [loop e1; to_type_hint ct p]
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
				begin match fst e1 with
				| EParenthesis (ECheckType (e2, (CTPath{tname="String";tpackage=[]},_)),_) -> expr "EConst" [mk_enum "Constant" "CString" [e2] (pos e2)]
				| EParenthesis (ECheckType (e2, (CTPath{tname="Int";tpackage=[]},_)),_) -> expr "EConst" [mk_enum "Constant" "CInt" [e2] (pos e2)]
				| EParenthesis (ECheckType (e2, (CTPath{tname="Float";tpackage=[]},_)),_) -> expr "EConst" [mk_enum "Constant" "CFloat" [e2] (pos e2)]
				| _ -> (ECall ((EField ((EField ((EField ((EConst (Ident "haxe"),p),"macro"),p),"Context"),p),"makeExpr"),p),[e; to_pos (pos e)]),p)
				end
			| Meta.Dollar "i", _ ->
				expr "EConst" [mk_enum "Constant" "CIdent" [e1] (pos e1)]
			| Meta.Dollar "p", _ ->
				(ECall ((EField ((EField ((EField ((EConst (Ident "haxe"),p),"macro"),p),"MacroStringTools"),p),"toFieldExpr"),p),[e]),p)
			| Meta.Pos, [pexpr] ->
				let old = !cur_pos in
				cur_pos := Some pexpr;
				let e = loop e1 in
				cur_pos := old;
				e
			| _ ->
				expr "EMeta" [to_obj [("name",to_string (Meta.to_string m) p);("params",to_expr_array ml p);("pos",to_pos p)] p;loop e1]
	and to_tparam_decl p t =
		to_obj [
			"name", to_placed_name t.tp_name;
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
				| HExtends t -> ext := (match !ext with
					| None -> Some (to_tpath t p)
					| Some _ -> begin
						impl := (to_tpath t p) :: !impl;
						!ext
						end)
				| HImplements i-> impl := (to_tpath i p) :: !impl
			) d.d_flags;
			to_obj [
				"pack", (EArrayDecl [],p);
				"name", to_string (fst d.d_name) (pos d.d_name);
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

let reify_expr e in_macro =
	let to_expr,_,_ = reify in_macro in
	let e = to_expr e in
	(ECheckType (e,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = None; tparams = [] },null_pos)),pos e)
