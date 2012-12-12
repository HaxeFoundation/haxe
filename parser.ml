(*
 *  Haxe Compiler
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
let doc = ref None
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
	| OpAssign | OpAssignOp _ -> 9, right

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

let lower_ident = parser
	| [< '(Const (Ident i),p) when is_lower_ident i >] -> i

let any_enum_ident = parser
	| [< i = ident >] -> i
	| [< '(Kwd k,p) when Filename.basename p.pfile = "StdTypes.hx" >] -> s_keyword k, p

let property_ident = parser
	| [< i, _ = ident >] -> i
	| [< '(Kwd Dynamic,_) >] -> "dynamic"
	| [< '(Kwd Default,_) >] -> "default"
	| [< '(Kwd Null,_) >] -> "null"

let get_doc s =
	let d = !doc in
	doc := None;
	d

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
	doc := None;
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
	| [< meta = parse_meta; c = parse_common_flags; s >] ->
		match s with parser
		| [< n , p1 = parse_enum_flags; doc = get_doc; name = type_name; tl = parse_constraint_params; '(BrOpen,_); l = plist parse_enum; '(BrClose,p2) >] ->
			(EEnum {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = List.map snd c @ n;
				d_data = l
			}, punion p1 p2)
		| [< n , p1 = parse_class_flags; doc = get_doc; name = type_name; tl = parse_constraint_params; hl = psep Comma parse_class_herit; '(BrOpen,_); fl, p2 = parse_class_fields false p1 >] ->
			(EClass {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = List.map fst c @ n @ hl;
				d_data = fl;
			}, punion p1 p2)
		| [< '(Kwd Typedef,p1); doc = get_doc; name = type_name; tl = parse_constraint_params; '(Binop OpAssign,p2); t = parse_complex_type; s >] ->
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
		| [< '(Kwd Abstract,p1); doc = get_doc; name = type_name; tl = parse_constraint_params; sl = psep Comma parse_abstract_relations; '(BrOpen,_); '(BrClose,p2) >] ->
			let flags = List.map (fun (_,c) -> match c with EPrivate -> APrivAbstract | EExtern -> error (Custom "extern abstract not allowed") p1) c in
			(EAbstract {
				d_name = name;
				d_doc = doc;
				d_meta = meta;
				d_params = tl;
				d_flags = flags @ sl;
				d_data = ();
			},punion p1 p2)

and parse_import s p1 =
	let rec loop acc =
		match s with parser
		| [< '(Dot,p) >] ->
			if is_resuming p then raise (TypePath (List.rev (List.map fst acc),None));
			(match s with parser
			| [< '(Const (Ident k),p) >] ->
				loop ((k,p) :: acc)
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
	| [< '(Binop OpLte,_); t = parse_complex_type >] -> ASuperType t
	| [< '(Binop OpAssign,p1); '(Binop OpGt,p2) when p1.pmax = p2.pmin; t = parse_complex_type >] -> ASubType t

and parse_package s = psep Dot lower_ident s

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
			| Kwd Public :: _ | Kwd Static :: _ | Kwd Var :: _ | Kwd Override :: _ | Kwd Dynamic :: _ | Kwd Inline :: _ ->
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
	| [< '(Const (Ident i),p) >] -> i, p
	| [< '(Kwd k,p) >] -> s_keyword k,p
	| [< '(DblDot,_); s >] -> let n, p = meta_name s in ":" ^ n, p

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
	| [< name, p = dollar_ident; s >] ->
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
				cff_meta = if opt then [":optional",[],p1] else [];
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
	doc := None;
	let meta = parse_meta s in
	match s with parser
	| [< name, p1 = any_enum_ident; doc = get_doc; params = parse_constraint_params; s >] ->
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
	doc := None;
	match s with parser
	| [< meta = parse_meta; al = parse_cf_rights true []; doc = get_doc; s >] ->
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
				| [< e = toplevel_expr >] -> Some e, pos e
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

and block2 name ident p = parser
	| [< '(DblDot,_); e = expr; l = parse_obj_decl >] -> EObjectDecl ((name,e) :: l)
	| [< e = expr_next (EConst ident,p); s >] ->
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

and expr = parser
	| [< (name,params,p) = parse_meta_entry; s >] ->
		(EMeta((name,params,p), expr s),p)
	| [< '(BrOpen,p1); b = block1; '(BrClose,p2); s >] ->
		let e = (b,punion p1 p2) in
		(match b with
		| EObjectDecl _ -> expr_next e s
		| _ -> e)
	| [< '(Const (Ident "macro"),p); s >] ->
		(match Stream.npeek 1 s with
		| [(DblDot,_)] ->
			(match s with parser
			| [< '(DblDot,_); t = parse_complex_type >] ->
				let t = snd (reify !in_macro) t p in
				(ECheckType (t,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = Some "ComplexType"; tparams = [] })),p)
			| [< >] -> serror())
		| [(_,p2)] when p2.pmin > p.pmax ->
			let reify e =
				let e = fst (reify !in_macro) e in
				(ECheckType (e,(CTPath { tpackage = ["haxe";"macro"]; tname = "Expr"; tsub = None; tparams = [] })),pos e)
			in
			(match s with parser
			| [< '(Kwd Var,p1); vl = psep Comma parse_var_decl >] -> reify (EVars vl,p1)
			| [< e = expr >] -> reify e
			| [< >] -> expr_next (EConst (Ident "macro"),p) s)
		| _ ->
			expr_next (EConst (Ident "macro"),p) s)
	| [< '(Kwd Var,p1); v = parse_var_decl >] -> (EVars [v],p1)
	| [< '(Const c,p); s >] -> expr_next (EConst c,p) s
	| [< '(Kwd This,p); s >] -> expr_next (EConst (Ident "this"),p) s
	| [< '(Kwd True,p); s >] -> expr_next (EConst (Ident "true"),p) s
	| [< '(Kwd False,p); s >] -> expr_next (EConst (Ident "false"),p) s
	| [< '(Kwd Null,p); s >] -> expr_next (EConst (Ident "null"),p) s
	| [< '(Kwd Callback,p); s >] -> expr_next (EConst (Ident "callback"),p) s
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
				(*
					we can't directly npeek 2 elements because this might
					remove some documentation tag.
				*)
				match Stream.npeek 1 s with
				| [(Semicolon,_)] ->
					(match Stream.npeek 2 s with
					| [(Semicolon,_); (Kwd Else,_)] ->
						Stream.junk s;
						Stream.junk s;
						Some (secure_expr s)
					| _ -> None)
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
	| [< (name,params,p) = parse_meta_entry; s >] ->
		(EMeta((name,params,p), expr_next e1 s),p)
	| [< '(Dot,p); s >] ->
		if is_resuming p then display (EDisplay (e1,false),p);
		(match s with parser
		| [< '(Const (Ident f),p2) when p.pmax = p2.pmin; s >] -> expr_next (EField (e1,f) , punion (pos e1) p2) s
		| [< '(Binop OpOr,p2) when do_resume() >] -> display (EDisplay (e1,false),p) (* help for debug display mode *)
		| [< >] ->
			(* turn an integer followed by a dot into a float *)
			match e1 with
			| (EConst (Int v),p2) when p2.pmax = p.pmin -> expr_next (EConst (Float (v ^ ".")),punion p p2) s
			| _ -> serror())
	| [< '(POpen,p1); s >] ->
		if is_resuming p1 then display (EDisplay (e1,true),p1);
		(match s with parser
		| [< '(Binop OpOr,p2) when do_resume() >] -> display (EDisplay (e1,false),p1) (* help for debug display mode *)
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
		let b = EBlock (try block [] s with Display e -> display (ESwitch (eswitch,cases,Some e),punion (pos eswitch) (pos e))) in
		let l , def = parse_switch_cases eswitch cases s in
		(match def with None -> () | Some (e,p) -> error Duplicate_default p);
		l , Some (b,p1)
	| [< '(Kwd Case,p1); el = psep Comma expr; eg = popt parse_guard; '(DblDot,_); s >] ->
		let b = EBlock (try block [] s with Display e -> display (ESwitch (eswitch,List.rev ((el,eg,e) :: cases),None),punion (pos eswitch) (pos e))) in
		parse_switch_cases eswitch ((el,eg,(b,p1)) :: cases) s
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

type small_type =
	| TNull
	| TBool of bool
	| TFloat of float
	| TString of string
	
let parse ctx code =
	let old = Lexer.save() in
	let old_cache = !cache in
	let mstack = ref [] in
	cache := DynArray.create();
	doc := None;
	in_macro := Common.defined ctx Common.Define.Macro;
	Lexer.skip_header code;

	let sraw = Stream.from (fun _ -> Some (Lexer.token code)) in
	let rec next_token() = process_token (Lexer.token code)

	and process_token tk =
		match fst tk with
		| Comment s ->
			if !use_doc then begin
				let l = String.length s in
				if l > 0 && s.[0] = '*' then doc := Some (String.sub s 1 (l - (if l > 1 && s.[l-1] = '*' then 2 else 1)));
			end;
			next_token()
		| CommentLine s ->
			next_token()
		| Macro "end" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				mstack := l;
				next_token())
		| Macro "else" | Macro "elseif" ->
			(match !mstack with
			| [] -> tk
			| _ :: l ->
				mstack := l;
				process_token (skip_tokens (snd tk) false))
		| Macro "if" ->
			process_token (enter_macro (snd tk))
		| Macro "error" ->
			(match Lexer.token code with
			| (Const (String s),p) -> error (Custom s) p
			| _ -> error Unimplemented (snd tk))
		| Macro "line" ->
			let line = (match next_token() with
				| (Const (Int s),_) -> int_of_string s
				| (t,p) -> error (Unexpected t) p
			) in
			!(Lexer.cur).Lexer.lline <- line - 1;
			next_token();
		| _ ->
			tk

	and enter_macro p =
		let is_true = function
			| TBool false | TNull | TFloat 0. | TString "" -> false
			| _ -> true
		in
		let cmp v1 v2 =
			match v1, v2 with
			| TNull, TNull -> 0
			| TFloat a, TFloat b -> compare a b
			| TString a, TString b -> compare a b
			| TBool a, TBool b -> compare a b
			| TString a, TFloat b -> compare (float_of_string a) b
			| TFloat a, TString b -> compare a (float_of_string b)
			| _ -> raise Exit (* always false *)
		in
		let rec loop (e,p) =
			match e with
			| EConst (Ident i) ->
				(try TString (Common.raw_defined_value ctx i) with Not_found -> TNull)
			| EConst (String s) -> TString s
			| EConst (Int i) -> TFloat (float_of_string i)
			| EConst (Float f) -> TFloat (float_of_string f)
			| EBinop (OpBoolAnd, e1, e2) -> TBool (is_true (loop e1) && is_true (loop e2))
			| EBinop (OpBoolOr, e1, e2) -> TBool (is_true (loop e1) || is_true(loop e2))
			| EUnop (Not, _, e) -> TBool (not (is_true (loop e)))
			| EParenthesis e -> loop e
			| EBinop (op, e1, e2) ->
				let v1 = loop e1 in
				let v2 = loop e2 in
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
				| _ -> error (Custom "Insupported operation") p)
			| _ ->
				error Unclosed_macro p
		in
		let tk, e = parse_macro_cond false sraw in
		let tk = (match tk with None -> Lexer.token code | Some tk -> tk) in
		if is_true (loop e) || (match fst e with EConst (Ident "macro") when Common.unique_full_path p.pfile = (!resume_display).pfile -> true | _ -> false) then begin
			mstack := p :: !mstack;
			tk
		end else
			skip_tokens_loop p true tk

	and skip_tokens_loop p test tk =
		match fst tk with
		| Macro "end" ->
			Lexer.token code
		| Macro "elseif" | Macro "else" when not test ->
			skip_tokens p test
		| Macro "else" ->
			mstack := snd tk :: !mstack;
			Lexer.token code
		| Macro "elseif" ->
			enter_macro (snd tk)
		| Macro "if" ->
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
